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
  library(ggplot2)
  library(ggforce)
  library(tidyr)
  library(stringr)
  library(scales)
})

# Brings in t_radius(), log_G_t(), p_G_t(), optimal_g(), av_tidy() -- the same
# anytime-valid math used throughout the rest of the paper's simulations, so
# this real-data case study is guaranteed to use identical formulas.
source(here("code", "cj.R"))
source(here("code", "figure_style.R"))

asylum_long <- read_csv(here("data", "figure5.csv"), show_col_types = FALSE)

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

# cat(sprintf("Total respondents (clusters): G = %d\n", G_total))
# cat(sprintf("g* minimizing the CS radius at G = %d: %.4f\n", G_total, g_star))

## ---- Final fit: conventional (fixed-n) vs. anytime-valid ------------------

final_model <- feols(amce_formula, data = asylum_long, cluster = ~ResponseId)

final_conventional <- broom::tidy(final_model, conf.int = TRUE, conf.level = 1 - alpha) |>
  filter(term != "(Intercept)") |>
  transmute(term, estimate, std.error, conf.low, conf.high, method = "Conventional (CI)")

final_av <- av_tidy(final_model, alpha = alpha, cluster = asylum_long$ResponseId, g = g_star) |>
  filter(term != "(Intercept)") |>
  transmute(term, estimate, std.error, conf.low, conf.high, method = "Anytime-valid (CS)")

# Add on reference levels
predictors <- attr(terms(final_model), "term.labels")
reference_levels <- vapply(
  predictors,
  \(var) paste0(var, levels(as.factor(asylum_long[[var]]))[[1]]),
  character(1)
)
reference_levels_df <- bind_rows(
  tibble(term = reference_levels, method = "Conventional (CI)"),
  tibble(term = reference_levels, method = "Anytime-valid (CS)")
)

final_comparison <- bind_rows(final_conventional, final_av, reference_levels_df) |>
  mutate(across(where(is.numeric), \(x) ifelse(is.na(x), 0., x))) |>
  arrange(method, term)

# cat(sprintf("Final fit has %d non-intercept AMCE coefficients.\n", n_distinct(final_comparison$term)))

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

# cat(sprintf(
#   "Sequential refitting: %d fits from G = %d to G = %d (step = %d)\n",
#   length(G_grid), min(G_grid), max(G_grid), step_size
# ))

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
# cat(sprintf("Sequential refitting took %.1f seconds.\n", elapsed))

# cat(sprintf(
#   "Wrote sequential estimates for G = %d..%d (%d model fits) to data/sequential_av_estimates.csv\n",
#   min(G_grid), max(G_grid), length(G_grid)
# ))

# Written last (not right after g_star is computed) so it can record the
# actual measured runtime of the sequential refitting loop above, which
# report.Rmd cites verbatim rather than restating from memory.
meta <- tibble(
  G_total = G_total,
  n_obs = nrow(asylum_long),
  alpha = alpha,
  g_star = g_star,
  step_size = step_size,
  n_fits = length(G_grid),
  elapsed_sec = elapsed
)

## ----  Plot Figure 6 --------------------------------------------------------

# Prep plot data
attribute_display <- c(
  cconsist  = "Asylum testimony",
  cgender   = "Gender",
  corigin   = "Country of origin",
  cage      = "Age",
  cjob      = "Previous occupation",
  cvulner   = "Vulnerability",
  creason   = "Reason for migrating",
  creligion = "Religion",
  clang     = "Language skills"
)
attribute_vars <- names(attribute_display)

figure_6_data <- separate_wider_delim(
    data = final_comparison,
    cols = "term",
    delim = regex("(?<=[a-z])(?=[A-Z0-9])"),
    names = c("varname", "term")
  ) |>
  mutate(
    varname = recode_values(
      varname,
      from = names(attribute_display),
      to = attribute_display
    ),
    varname = factor(
      varname,
      levels = c(
        "Asylum testimony",
        "Gender",
        "Country of origin",
        "Age",
        "Previous occupation",
        "Vulnerability",
        "Reason for migrating",
        "Religion",
        "Language skills"
      ),
      ordered = TRUE
    )
  )

# Plot it!
asylum_estimates_plot <- ggplot(
  figure_6_data,
  aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high, color = method)
) +
  geom_hline(yintercept = 0, linetype = "solid", color = "gray70") +
  geom_pointrange(
    position = position_dodge(width = 0.6),
    size = 0.2
  ) +
  coord_flip() +
  facet_wrap(~ varname, ncol = 1, drop = TRUE, scales = "free_y") +
  scale_color_manual(values = method_palette) +
  scale_y_continuous(labels = label_percent()) +
  labs(x = NULL, y = "AMCE estimate (95% interval)", color = NULL) +
  conjoint_theme(base_size = 11)

asylum_estimates_plot <- ggplot(
  figure_6_data,
  aes(
    y = term,
    x = estimate,
    xmin = conf.low,
    xmax = conf.high,
    color = method
  )
) +
  geom_vline(
    xintercept = 0,
    linetype = "solid",
    color = "gray70"
  ) +
  geom_pointrange(
    position = position_dodge(width = 0.6),
    orientation = "y",
    size = 0.15
  ) +
  facet_col(
    facets = vars(varname),
    scales = "free_y",
    space = "free",
    strip.position = "top",
    drop = TRUE
  ) +
  scale_color_manual(values = method_palette) +
  scale_x_continuous(labels = label_percent()) +
  scale_y_discrete(limits = rev) +
  labs(
    x = "AMCE estimate (95% interval)",
    y = NULL,
    color = NULL
  ) +
  conjoint_theme(base_size = 11) +
  theme(
    panel.spacing.y = unit(0.05, "lines"),
    strip.text = element_text(face = "bold")
  )

save_paper_figure(
  "figure6.png",
  plot = asylum_estimates_plot,
  width = 6.5,
  height = 11,
  dpi = 500
)

## ----  Plot Figure 5 --------------------------------------------------------

term_attribute <- function(term) {
  attribute_vars[vapply(attribute_vars, function(a) startsWith(term, a), logical(1))][1]
}

all_terms <- final_comparison |>
  distinct(term) |>
  pull(term)
term_attr_map <- vapply(all_terms, term_attribute, character(1))
term_level_map <- substring(all_terms, nchar(term_attr_map) + 1)
term_labels <- setNames(
  paste0(attribute_display[term_attr_map], ":\n", term_level_map),
  all_terms
)

plot_data <- final_comparison |>
  mutate(
    label = factor(term_labels[term], levels = rev(term_labels[all_terms])),
    method = factor(method, levels = c("Conventional (CI)", "Anytime-valid (CS)"))
  )

final_fit_stats <- sequential_estimates |>
  filter(G == max(G)) |>
  mutate(attribute = vapply(term, term_attribute, character(1)))

curated_terms <- final_fit_stats |>
  group_by(attribute) |>
  slice_max(abs(statistic), n = 1, with_ties = FALSE) |>
  ungroup() |>
  arrange(match(attribute, attribute_vars)) |>
  pull(term)

threshold <- 1 / meta$alpha[[1]]

eprocess_data <- sequential_estimates |>
  filter(term %in% curated_terms) |>
  mutate(
    label = factor(term_labels[term], levels = term_labels[curated_terms]),
    log_e_value = log_G_t(statistic^2, G, meta$g_star[[1]])
  )

# Plot it!
eprocess_plot <- ggplot(
  eprocess_data,
  aes(x = G, y = log_e_value)
) +
  geom_line(color = paper_colors$av) +
  geom_hline(yintercept = log(threshold), linetype = "dashed", color = paper_colors$accent) +
  scale_x_continuous(labels = scales::label_comma()) +
  facet_wrap(~ label, ncol = 3, scales = "free_y") +
  labs(
    x = "Number of respondents (G)",
    y = "Log e-value"
  ) +
  conjoint_theme(base_size = 11)

save_paper_figure(
  "figure5.png",
  plot = eprocess_plot,
  width = 8,
  height = 6,
  dpi = 500
)
