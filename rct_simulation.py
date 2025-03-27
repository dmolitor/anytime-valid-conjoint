from io import StringIO
from joblib import Parallel, delayed
import numpy as np
import pandas as pd
from pathlib import Path
import plotnine as pn
import statsmodels.api as sm
from tqdm import tqdm
import warnings
warnings.filterwarnings("ignore")

from anytime_valid_conjoint.utils import (
    sequential_asymptotic_cs,
    sequential_f_cs
)

base_dir = Path(__file__).resolve().parent

def rct_draw(ate):
    """
    Generate a single random draw from an RCT.

    Parameters
    ----------
    ate : float
        The average treatment effect to be applied on the treatment indicator.

    Returns
    -------
    pandas.DataFrame
        A single-row DataFrame containing Y, X1, X2, X3, and W.
    """
    X1 = np.random.normal()
    X2 = np.random.normal()
    X3 = np.random.normal()
    W = np.random.binomial(1, 0.5)
    # Y is generated with a fixed intercept and effects of X1, X2, X3 plus noise.
    Y = 0.5 + ate * W - X1 + 1.5 * X2 - 0.4 * X3 + np.random.normal()
    return pd.DataFrame({"Y": [Y], "X1": [X1], "X2": [X2], "X3": [X3], "W": [W]})

def simulate(ate, n=1000, alpha=0.05):
    """
    Simulate an RCT and compute estimates for the treatment effect via three methods:
    (1) The "Fixed-n" model (using standard confidence intervals),
    (2) A sequential confidence sequence (via sequential_f_cs),
    (3) An asymptotic confidence sequence (via sequential_asymptotic_cs).

    Parameters
    ----------
    ate : float
        The average treatment effect to use in simulation.
    n : int, optional
        Number of additional draws to simulate (default is 1000).

    Returns
    -------
    pandas.DataFrame
        A DataFrame with columns for term, estimate, method (which), confidence sequence lower/upper bounds,
        and the current simulation index.
    """
    # Start with 10 initial draws.
    rct_data = pd.concat([rct_draw(ate) for _ in range(10)], ignore_index=True)
    estimates_list = []
    for i in range(1, int(n) + 1):
        # Append one new draw.
        new_draw = rct_draw(ate)
        rct_data = pd.concat([rct_data, new_draw], ignore_index=True)
        # Fit the model: Y ~ W + X1 + X2 + X3, include a constant.
        X = rct_data[["W", "X1", "X2", "X3"]]
        X = sm.add_constant(X)
        y = rct_data["Y"]
        model = sm.OLS(y, X).fit()
        # Extract coefficient estimates and confidence intervals.
        coefs = pd.read_html(
            StringIO(model.summary(alpha=alpha).tables[1].as_html()),
            header=0
        )[0]
        coefs.columns = [
            "term",
            "estimate",
            "std.error",
            "statistic",
            "p.value",
            "conf.low",
            "conf.high"
        ]
        coefs["which"] = "Fixed-n"
        # Compute sequential CS for all coefficients.
        seq_cs_df = sequential_f_cs(
            delta=coefs["estimate"].values,
            se=coefs["std.error"].values,
            n=model.nobs,
            n_params=len(model.params),
            Z=np.linalg.inv(model.cov_params().values),
            alpha=alpha,
            phi=1,
            term=coefs["term"].values
        )
        seq_cs_df["which"] = "Sequential"
        # Compute the asymptotic CS only for the treatment effect "W".
        w_coef = coefs[coefs["term"] == "W"]
        asymptotic_cs_df = sequential_asymptotic_cs(
            delta=w_coef["estimate"].values,
            n=model.nobs,
            propensity=0.5,
            lambda_param=100,
            alpha=0.05,
            sigma_hat=np.sqrt(model.mse_resid),
            term=w_coef["term"].values
        )
        asymptotic_cs_df["which"] = "Asymptotic"
        # Combine only rows corresponding to "W" from fixed-n, sequential, and asymptotic.
        fixed_w = coefs[coefs["term"] == "W"][["term", "estimate", "which", "conf.low", "conf.high"]]
        fixed_w = fixed_w.rename(columns={"conf.low": "cs_lower", "conf.high": "cs_upper"})
        seq_w = seq_cs_df[seq_cs_df["term"] == "W"][["term", "estimate", "which", "cs_lower", "cs_upper"]]
        asymptotic_w = asymptotic_cs_df[["term", "estimate", "which", "cs_lower", "cs_upper"]]
        # Combine the three methods.
        combined = pd.concat([fixed_w, seq_w, asymptotic_w], ignore_index=True)
        combined["index"] = i
        estimates_list.append(combined)
    # Combine all simulation iterations.
    rct_estimates = pd.concat(estimates_list, ignore_index=True)
    return rct_estimates


# Simulate RCT with ATE = 0.
print("Simulating RCT with ATE = 0 ...")
sim_results = simulate(ate=0)

# Plot the simulation results.
(
    pn.ggplot(sim_results, pn.aes(x="index", y="estimate", ymin="cs_lower", ymax="cs_upper", color="which")) +
    pn.geom_line() +
    pn.geom_linerange(alpha = 0.1) +
    pn.geom_hline(yintercept=0, linetype="dashed", color="black") +
    pn.coord_cartesian(ylim=(-3, 3)) +
    pn.theme_minimal() +
    pn.labs(x = "Sample size (N)", y = "ATE", color = "")
).save(
    base_dir / "figures" / "rct_simulation_py.png",
    width = 5,
    height = 4,
    dpi = 300
)

# Type I error simulations.
print("Type 1 error simulations ...")
n_sim = 10
def run_simulation(sim_i):
    """
    Run a single simulation and add a simulation index.

    Parameters
    ----------
    sim_i : int
        Simulation index.

    Returns
    -------
    pandas.DataFrame
        Simulation results with an added column 'sim'.
    """
    results = simulate(ate=0, n=1000)
    results["sim"] = sim_i
    return results

# Use joblib to run the simulations in parallel.
sim_results_list = [
    r for r in
    tqdm(
        Parallel(return_as="generator", n_jobs=-1)(delayed(run_simulation)(i) for i in range(n_sim)),
        total=n_sim
    )
]
sim_results_all = pd.concat(sim_results_list, ignore_index=True)

# Calculate Type I error rates.
print("Calculating Type I error rate ...")
sim_results_all["covered"] = (
    (sim_results_all["cs_lower"] <= 0)
    & (0 <= sim_results_all["cs_upper"])
)
grouped = (
    sim_results_all
    .groupby(["sim", "which"])["covered"]
    .apply(lambda x: not x.all())
    .reset_index(name="error")
)
error_rates = (
    grouped
    .groupby("which")["error"]
    .mean()
    .reset_index()
    .rename(columns={"error": "error_rate"})
)
print(error_rates)
