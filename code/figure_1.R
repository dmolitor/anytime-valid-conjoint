suppressPackageStartupMessages({
  library(dplyr)
  library(here)
  library(ggplot2)
  library(readr)
})

source(here("code", "cj.R"))
source(here("code", "figure_style.R"))

options(future.globals.maxSize = Inf)

## Figure 1 -------------------------------------------------------------------

# Specify utility coefficients for the nonlinear forced-choice DGP.
# The exact AMCEs are computed by finite summation in `compute_true_amces()`.
utility_effects <- list(
  Party = c("Left" = 0.35),
  Region = c("South" = -0.06, "East" = -0.27, "West" = 0.16)
)
utility_interactions <- matrix(
  c(0, 0, 0, 0,
    0, 0.04, -0.05, 0.03),
  2, 4,
  byrow = TRUE,
  dimnames = list(c("Right", "Left"), c("North", "South", "East", "West"))
)

# Specify conjoint parameters
tasks_per_respondent = 2
number_of_respondents = 2500
significance_level = 0.05

# Define the conjoint setup
cj <- ConjointSim$new(
  levels = list(
    Party = c("Right" = 1/2, "Left" = 1/2),
    Region = c("North" = 1/4, "South" = 1/4, "East" = 1/4, "West" = 1/4)
  ),
  amces = utility_effects,
  interactions = utility_interactions,
  n_tasks = tasks_per_respondent,
  dgp = "logit"
)
# Simulate the conjoint
set.seed(163373)
out <- cj$simulate_conjoint(
  alpha = significance_level,
  experiment_size = number_of_respondents,
  chunk_size = 100
)

# Plot Figure 1
suppressWarnings({
  plot_data <- cj$estimates |>
    mutate(stat_sig = conf.low > 0 | conf.high < 0) |>
    group_by(attribute, level) |>
    mutate(
      true_from_here_on = rev(cumall(rev(stat_sig))),
      first_stat_sig = if (any(true_from_here_on)) min(i[true_from_here_on]) else NA_integer_
    ) |>
    ungroup()

  truth_lines <- plot_data |>
    distinct(attribute, level, amce)

  p <- ggplot(
      plot_data,
      aes(x = i, y = estimate, ymin = conf.low, ymax = conf.high)
    ) +
    geom_ribbon(fill = paper_colors$av, alpha = 0.16) +
    geom_line(color = paper_colors$av, linewidth = 0.45) +
    geom_vline(
      aes(xintercept = first_stat_sig),
      linetype = "dashed",
      color = paper_colors$conventional,
      linewidth = 0.35
    ) +
    geom_hline(
      aes(yintercept = amce),
      data = truth_lines,
      color = paper_colors$accent,
      linetype = "dashed",
      linewidth = 0.35
    ) +
    geom_hline(yintercept = 0, linetype = "dotted", color = paper_colors$reference) +
    facet_wrap(~ paste0(attribute, " - ", level), ncol = 2) +
    coord_cartesian(ylim = c(-0.25, 0.25)) +
    labs(x = "Respondent clusters (G)", y = "AMCE") +
    conjoint_theme()

  # Save the results
  save_paper_figure(
    "figure1.png",
    plot = p,
    width = 8,
    height = 5
  )
})
