# Fits the AMCE linear-probability model to the candidate-gender conjoint
# data (Teele, Kalla & Rosenbluth, 2018) both at the final sample size
# (conventional fixed-n vs. anytime-valid) and sequentially as respondents
# accumulate, to trace out how the confidence sequence and e-value for each
# AMCE -- especially candidate gender, the attribute of substantive interest
# for this paper -- evolve.
suppressPackageStartupMessages({
  library(broom)
  library(dplyr)
  library(fixest)
  library(here)
  library(readr)
})

# Brings in t_radius(), log_G_t(), p_G_t(), optimal_g(), av_tidy() -- the same
# anytime-valid math used throughout the rest of the paper's simulations, so
# this real-data case study is guaranteed to use identical formulas.
source(here("code", "cj.R"))

candidate_long <- read_csv(here("candidate_gender_case_study", "data", "candidate_long.csv"), show_col_types = FALSE)

amce_formula <- winner ~ orig_1ys + orig_3ys + orig_8ys +
  orig_FM_sp + orig_MD_sp +
  orig_law + orig_may + orig_leg +
  orig_1ch + orig_3ch +
  orig_45 + orig_65 +
  orig_cand_female

alpha <- 0.05
G_total <- n_distinct(candidate_long$responseid)

# g is fixed once, in advance, at the value that minimizes the confidence-
# sequence radius for the planned final number of clusters (G_total). It is
# NOT re-optimized as data accumulate -- doing so would break anytime-
# validity, since g must not depend on how the sequential monitoring turns
# out.
g_star <- optimal_g(G_total, alpha)

cat(sprintf("Total respondents (clusters): G = %d\n", G_total))
cat(sprintf("g* minimizing the CS radius at G = %d: %.4f\n", G_total, g_star))

tibble(
  G_total = G_total,
  n_obs = nrow(candidate_long),
  alpha = alpha,
  g_star = g_star
) |>
  write_csv(here("candidate_gender_case_study", "data", "meta.csv"))

## ---- Final fit: conventional (fixed-n) vs. anytime-valid ------------------

final_model <- feols(amce_formula, data = candidate_long, cluster = ~responseid)

final_conventional <- broom::tidy(final_model, conf.int = TRUE, conf.level = 1 - alpha) |>
  filter(term != "(Intercept)") |>
  transmute(term, estimate, std.error, conf.low, conf.high, method = "Conventional (fixed-n)")

final_av <- av_tidy(final_model, alpha = alpha, cluster = candidate_long$responseid, g = g_star) |>
  filter(term != "(Intercept)") |>
  transmute(term, estimate, std.error, conf.low, conf.high, method = "Anytime-valid (CS)")

final_comparison <- bind_rows(final_conventional, final_av)

write_csv(final_comparison, here("candidate_gender_case_study", "data", "final_comparison.csv"))

## ---- Sequential monitoring: refit as respondents accumulate ---------------

# Respondents are processed in resp_order (their order of first appearance in
# the raw file), used here as a stand-in for their arrival order -- the raw
# data carry no survey timestamp. G_step ranges from 5 clusters (below which
# the cluster-robust sandwich is too noisy to be meaningful) up to G_total.
# A timing check (fitting the full grid at step = 1 took well under a
# minute) confirmed refitting at every single respondent is feasible here,
# so no coarser step is needed.
G_grid <- seq(5, G_total)

sequential_estimates <- bind_rows(lapply(G_grid, function(G_step) {
  data_so_far <- filter(candidate_long, resp_order <= G_step)
  model_step <- feols(amce_formula, data = data_so_far, cluster = ~responseid)
  av_tidy(model_step, alpha = alpha, cluster = data_so_far$responseid, g = g_star) |>
    filter(term != "(Intercept)") |>
    mutate(
      G = G_step,
      e_value = exp(log_G_t(statistic^2, G, g_star))
    ) |>
    select(G, term, estimate, std.error, statistic, p.value, e_value, conf.low, conf.high)
}))

write_csv(sequential_estimates, here("candidate_gender_case_study", "data", "sequential_av_estimates.csv"))

cat(sprintf(
  "Wrote sequential estimates for G = %d..%d (%d model fits) to data/sequential_av_estimates.csv\n",
  min(G_grid), max(G_grid), length(G_grid)
))
