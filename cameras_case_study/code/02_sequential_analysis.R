# Fits the AMCE linear-probability model to the digital-camera conjoint data
# (bayesm::camera) both at the final sample size (conventional fixed-n vs.
# anytime-valid) and sequentially as respondents accumulate, to trace out how
# the confidence sequence and e-value for each AMCE evolve.
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

camera_long <- read_csv(here("cameras_case_study", "data", "camera_long.csv"), show_col_types = FALSE)

amce_formula <- chosen ~ canon + sony + nikon + panasonic +
  pixels + zoom + video + swivel + wifi + price

alpha <- 0.05
G_total <- n_distinct(camera_long$resp_id)

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
  n_obs = nrow(camera_long),
  alpha = alpha,
  g_star = g_star
) |>
  write_csv(here("cameras_case_study", "data", "meta.csv"))

## ---- Final fit: conventional (fixed-n) vs. anytime-valid ------------------

final_model <- feols(amce_formula, data = camera_long, cluster = ~resp_id)

final_conventional <- broom::tidy(final_model, conf.int = TRUE, conf.level = 1 - alpha) |>
  filter(term != "(Intercept)") |>
  transmute(term, estimate, std.error, conf.low, conf.high, method = "Conventional (fixed-n)")

final_av <- av_tidy(final_model, alpha = alpha, cluster = camera_long$resp_id, g = g_star) |>
  filter(term != "(Intercept)") |>
  transmute(term, estimate, std.error, conf.low, conf.high, method = "Anytime-valid (CS)")

final_comparison <- bind_rows(final_conventional, final_av)

write_csv(final_comparison, here("cameras_case_study", "data", "final_comparison.csv"))

## ---- Sequential monitoring: refit as respondents accumulate ---------------

# Respondents are processed in the order bayesm::camera stores them (resp_id
# 1, 2, 3, ...), used here as a stand-in for their arrival order -- the raw
# data carry no survey timestamp. G_step ranges from 5 clusters (below which
# the cluster-robust sandwich is too noisy to be meaningful) up to G_total.
G_grid <- seq(5, G_total)

sequential_estimates <- bind_rows(lapply(G_grid, function(G_step) {
  data_so_far <- filter(camera_long, resp_id <= G_step)
  model_step <- feols(amce_formula, data = data_so_far, cluster = ~resp_id)
  av_tidy(model_step, alpha = alpha, cluster = data_so_far$resp_id, g = g_star) |>
    filter(term != "(Intercept)") |>
    mutate(
      G = G_step,
      e_value = exp(log_G_t(statistic^2, G, g_star))
    ) |>
    select(G, term, estimate, std.error, statistic, p.value, e_value, conf.low, conf.high)
}))

write_csv(sequential_estimates, here("cameras_case_study", "data", "sequential_av_estimates.csv"))

cat(sprintf(
  "Wrote sequential estimates for G = %d..%d (%d model fits) to data/sequential_av_estimates.csv\n",
  min(G_grid), max(G_grid), length(G_grid)
))
