# Fits the AMCE linear-probability model to the immigration conjoint data
# (cjoint::immigrationconjoint) both at the final sample size (conventional
# fixed-n vs. anytime-valid) and sequentially as respondents accumulate, to
# trace out how the confidence sequence and e-value for each AMCE evolve.
suppressPackageStartupMessages({
  library(broom)
  library(cjoint)
  library(dplyr)
  library(fixest)
  library(here)
  library(readr)
})

# Brings in t_radius(), log_G_t(), p_G_t(), optimal_g(), av_tidy() -- the same
# anytime-valid math used throughout the rest of the paper's simulations, so
# this real-data case study is guaranteed to use identical formulas.
source(here("code", "cj.R"))

immigration_long <- read_csv(here("immigration_case_study", "data", "immigration_long.csv"), show_col_types = FALSE)

# CSV has no notion of factor levels, so every attribute column round-trips
# through data/immigration_long.csv as a plain character vector. Left as-is,
# feols() would build dummy variables using the *alphabetically* first level
# as the reference category for each attribute (e.g. "China" for Country of
# Origin, "child care provider" for Job) -- not the reference levels cjoint
# actually assigned (e.g. "India", "janitor"), which match the categories
# Hainmueller, Hopkins & Yamamoto (2014) treat as baseline throughout the
# paper. We restore the package's original factor level order here, before
# any model is fit, so every coefficient below is the AMCE relative to the
# same baseline used in the source literature.
attr_cols <- c(
  "Gender", "Education", "Language Skills", "Country of Origin", "Job",
  "Job Experience", "Job Plans", "Reason for Application", "Prior Entry"
)
data(immigrationconjoint, package = "cjoint")
for (col in attr_cols) {
  immigration_long[[col]] <- factor(immigration_long[[col]], levels = levels(immigrationconjoint[[col]]))
}
rm(immigrationconjoint)

# This is cjoint's own documented formula for this dataset (see
# ?cjoint::immigrationconjoint), used verbatim. All 9 attributes are
# multi-level categorical factors, so this expands to ~41 non-intercept
# dummy coefficients (vs. 10 simple binary attributes in the camera study).
amce_formula <- Chosen_Immigrant ~ Gender + Education + `Language Skills` +
  `Country of Origin` + Job + `Job Experience` + `Job Plans` +
  `Reason for Application` + `Prior Entry`

alpha <- 0.05
G_total <- n_distinct(immigration_long$CaseID)

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
  n_obs = nrow(immigration_long),
  alpha = alpha,
  g_star = g_star
) |>
  write_csv(here("immigration_case_study", "data", "meta.csv"))

## ---- Final fit: conventional (fixed-n) vs. anytime-valid ------------------

final_model <- feols(amce_formula, data = immigration_long, cluster = ~CaseID)

final_conventional <- broom::tidy(final_model, conf.int = TRUE, conf.level = 1 - alpha) |>
  filter(term != "(Intercept)") |>
  transmute(term, estimate, std.error, statistic, conf.low, conf.high, method = "Conventional (fixed-n)")

final_av <- av_tidy(final_model, alpha = alpha, cluster = immigration_long$CaseID, g = g_star) |>
  filter(term != "(Intercept)") |>
  transmute(term, estimate, std.error, statistic, conf.low, conf.high, method = "Anytime-valid (CS)")

# final_comparison keeps ALL ~41 non-intercept coefficients (every level of
# every attribute) -- nothing is curated or dropped here. The curated
# 9-coefficient subset used for the "evolving over time" figures is selected
# later, in report.Rmd, directly from this full set.
final_comparison <- bind_rows(final_conventional, final_av)

write_csv(final_comparison, here("immigration_case_study", "data", "final_comparison.csv"))

cat(sprintf("Wrote %d rows (%d terms x 2 methods) to data/final_comparison.csv\n",
  nrow(final_comparison), n_distinct(final_comparison$term)))

## ---- Sequential monitoring: refit as respondents accumulate ---------------

# Respondents are processed in resp_seq order (their order of first
# appearance in cjoint::immigrationconjoint, which is ascending CaseID),
# used here as a stand-in for their arrival order -- the raw data carry no
# survey timestamp. G_step ranges from 5 clusters (below which the
# cluster-robust sandwich is too noisy to be meaningful) up to G_total.
G_grid <- seq(5, G_total)

sequential_estimates <- bind_rows(lapply(G_grid, function(G_step) {
  data_so_far <- filter(immigration_long, resp_seq <= G_step)
  model_step <- feols(amce_formula, data = data_so_far, cluster = ~CaseID)
  av_tidy(model_step, alpha = alpha, cluster = data_so_far$CaseID, g = g_star) |>
    filter(term != "(Intercept)") |>
    mutate(
      G = G_step,
      e_value = exp(log_G_t(statistic^2, G, g_star))
    ) |>
    select(G, term, estimate, std.error, statistic, p.value, e_value, conf.low, conf.high)
}))

write_csv(sequential_estimates, here("immigration_case_study", "data", "sequential_av_estimates.csv"))

cat(sprintf(
  "Wrote sequential estimates for G = %d..%d (%d model fits) to data/sequential_av_estimates.csv\n",
  min(G_grid), max(G_grid), length(G_grid)
))
