suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(readr)
  library(here)
  library(scales)
})

source(here("code", "figure_style.R"))

n_sim <- 1000
significance_level <- 0.05

coverage_sim <- read_csv(here("data", "figure5.csv"), show_col_types = FALSE)

# Calculate error rates
error_rates <- coverage_sim |>
  mutate(covered = amce >= conf.low & amce <= conf.high) |>
  summarize(
    error = !all(covered),
    .by = c(sim_iter, attribute, level, amce)
  ) |>
  summarize(
    error_rate = mean(error),
    error_rate_se = sd(error)/sqrt(n_sim),
    .by = c(attribute, level, amce)
  )

suppressWarnings({

  # Plot coverage error rates
  error_rates_plot <- error_rates |>
    mutate(
      label = case_when(
        attribute == "Party" ~ paste("Party -", level),
        attribute == "Region" ~ paste("Region -", level)
      ),
      upper = error_rate + qnorm(1-significance_level/2)*error_rate_se,
      lower = error_rate - qnorm(1-significance_level/2)*error_rate_se
    ) |>
    ggplot(aes(
      x = label,
      y = error_rate,
      ymin = lower,
      ymax = upper
    )) +
    geom_point(color = paper_colors$av, position = position_dodge(width = 0.25), size = 1.5) +
    geom_linerange(color = paper_colors$av, position = position_dodge(width = 0.25), linewidth = 0.45) +
    geom_hline(yintercept = significance_level, linetype = "dashed", color = paper_colors$reference) +
    conjoint_theme() +
    coord_flip() +
    scale_y_continuous(
      labels = percent,
      breaks = seq(0, 0.075, length.out = 4),
      limits = c(0, 0.075)
    ) +
    labs(x = "", y = "Coverage error rate")

  # Save plot
  save_paper_figure(
    "figure5.png",
    plot = error_rates_plot,
    width = 5,
    height = 2.6
  )

})
