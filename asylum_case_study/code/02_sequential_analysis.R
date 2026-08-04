# Fits the AMCE linear-probability model to the 2016 Bansak, Hainmueller &
# Hangartner asylum-seeker conjoint data, both at the final sample size
# (conventional fixed-n vs. anytime-valid) and sequentially as respondents
# accumulate, to trace out how the confidence sequence and e-value for each
# AMCE evolve.
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

asylum_long <- read_csv(here("asylum_case_study", "data", "asylum_long.csv"), show_col_types = FALSE)

amce_formula <- pref ~ cconsist + cgender + corigin + cage + cjob +
  cvulner + creason + creligion + clang

alpha <- 0.05
G_total <- n_distinct(asylum_long$ResponseId)

# g is fixed once, in advance, at the value that minimizes the confidence-
# sequence radius for the planned final number of clusters (G_total). It is
# NOT re-optimized as data accumulate -- doing so would break anytime-
# validity, since g must not depend on how the sequential monitoring turns
# out.
g_star <- optimal_g(G_total, alpha)

cat(sprintf("Total respondents (clusters): G = %d\n", G_total))
cat(sprintf("g* minimizing the CS radius at G = %d: %.4f\n", G_total, g_star))

## ---- Final fit: conventional (fixed-n) vs. anytime-valid ------------------

final_model <- feols(amce_formula, data = asylum_long, cluster = ~ResponseId)

final_conventional <- broom::tidy(final_model, conf.int = TRUE, conf.level = 1 - alpha) |>
  filter(term != "(Intercept)") |>
  transmute(term, estimate, std.error, conf.low, conf.high, method = "Conventional (fixed-n)")

final_av <- av_tidy(final_model, alpha = alpha, cluster = asylum_long$ResponseId, g = g_star) |>
  filter(term != "(Intercept)") |>
  transmute(term, estimate, std.error, conf.low, conf.high, method = "Anytime-valid (CS)")

final_comparison <- bind_rows(final_conventional, final_av)

write_csv(final_comparison, here("asylum_case_study", "data", "final_comparison.csv"))

cat(sprintf("Final fit has %d non-intercept AMCE coefficients.\n", n_distinct(final_comparison$term)))

## ---- Sequential monitoring: refit as respondents accumulate ---------------

# Respondents are processed in resp_index order -- the order their 10-row
# blocks (5 tasks x 2 profiles) appear in the raw file -- as a stand-in for
# arrival order (see 01_prepare_data.R for why). With G_total = 18,021
# respondents, refitting at every single respondent (as cameras_case_study
# does, with only 332) would mean ~18,000 feols() calls plus ~18,000
# dplyr::filter() passes over a 180k-row frame. A short timing test (10 fits
# spread across the full range of G, including the filter step) measured
# ~0.02-0.13 seconds per fit, growing mildly with G. Stepping by 100
# respondents gives 182 total fits -- within the targeted 150-400 refits --
# projected at well under a minute total, comfortably inside the ~3 minute
# budget, while still resolving how each AMCE's confidence sequence and
# e-value evolve. G_step ranges from 5 clusters (below which the
# cluster-robust sandwich is too noisy to be meaningful) up to G_total, and
# G_total is always included so the last sequential fit exactly reproduces
# the final fit above.
step_size <- 100
G_grid <- sort(unique(c(seq(5, G_total, by = step_size), G_total)))

cat(sprintf(
  "Sequential refitting: %d fits from G = %d to G = %d (step = %d)\n",
  length(G_grid), min(G_grid), max(G_grid), step_size
))

t_start <- Sys.time()

sequential_estimates <- bind_rows(lapply(G_grid, function(G_step) {
  data_so_far <- filter(asylum_long, resp_index <= G_step)
  model_step <- feols(amce_formula, data = data_so_far, cluster = ~ResponseId)
  av_tidy(model_step, alpha = alpha, cluster = data_so_far$ResponseId, g = g_star) |>
    filter(term != "(Intercept)") |>
    mutate(
      G = G_step,
      e_value = exp(log_G_t(statistic^2, G, g_star))
    ) |>
    select(G, term, estimate, std.error, statistic, p.value, e_value, conf.low, conf.high)
}))

elapsed <- as.numeric(Sys.time() - t_start, units = "secs")
cat(sprintf("Sequential refitting took %.1f seconds.\n", elapsed))

write_csv(sequential_estimates, here("asylum_case_study", "data", "sequential_av_estimates.csv"))

cat(sprintf(
  "Wrote sequential estimates for G = %d..%d (%d model fits) to data/sequential_av_estimates.csv\n",
  min(G_grid), max(G_grid), length(G_grid)
))

# Written last (not right after g_star is computed) so it can record the
# actual measured runtime of the sequential refitting loop above, which
# report.Rmd cites verbatim rather than restating from memory.
tibble(
  G_total = G_total,
  n_obs = nrow(asylum_long),
  alpha = alpha,
  g_star = g_star,
  step_size = step_size,
  n_fits = length(G_grid),
  elapsed_sec = elapsed
) |>
  write_csv(here("asylum_case_study", "data", "meta.csv"))
