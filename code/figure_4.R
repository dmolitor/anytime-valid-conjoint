suppressPackageStartupMessages({
  library(ggplot2)
  library(here)
  library(readr)
  library(scales)
  library(dplyr)
})

power_df <- read_csv(here("data", "figure4.csv"), show_col_types = FALSE)

power_df <- power_df |>
  filter(attribute == "Region") |>
  mutate(
    stat_sig = 0 < conf.low | 0 > conf.high,
    overshoot_ratio = round(amce / target, 2)
  ) |>
  group_by(attribute, level, sim_iter, amce, target) |>
  summarize(
    type2_error = all(!stat_sig),
    early_stop = if (any(stat_sig)) {
      i[min(which(stat_sig))]
    } else {
      first(fixed_n)
    },
    fixed_n = first(fixed_n),
    overshoot_ratio = first(overshoot_ratio),
    .groups = "drop_last"
  ) |>
  ungroup() |>
  # We group by overshoot_ratio to summarize across all target AMCEs
  # If we want separate curves by each target AMCE, do the following:
  # group_by(attribute, level, amce, target)
  group_by(attribute, level, overshoot_ratio) |>
  summarize(
    power = mean(!type2_error),
    power_se = sd(!type2_error)/sqrt(n()),
    power_lb = power - 1.96*power_se,
    power_ub = power + 1.96*power_se,
    pct_sample_save = mean(1 - early_stop/fixed_n),
    pct_sample_save_se = sd(1 - early_stop/fixed_n)/sqrt(n()),
    pct_sample_save_lb = pct_sample_save - 1.96*pct_sample_save_se,
    pct_sample_save_ub = pct_sample_save + 1.96*pct_sample_save_se,
    fixed_n = first(fixed_n),
    n = n(),
    .groups = "drop_last"
  ) |>
  ungroup()

# Plot it

suppressWarnings({

  efficiency_plot <- ggplot(
    power_df,
    aes(
      x = overshoot_ratio,
      y = pct_sample_save,
      ymin = pct_sample_save_lb,
      ymax = pct_sample_save_ub
    )
  ) +
    geom_ribbon(alpha = 0.25) +
    geom_line(linewidth = 0.3) +
    geom_point(size = 1) +
    scale_y_continuous(
      labels = scales::percent_format(accuracy = 1)
    ) +
    scale_x_continuous(breaks = seq(1.25, 3, by = 0.25)) +
    labs(
      x = "True AMCE \u00F7 Powered-for AMCE",
      y = "Mean sample savings"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      legend.position = "none",
      panel.grid.minor = element_blank()
    )

  ggsave(
    here("figures", "figure4.png"),
    plot = efficiency_plot,
    dpi = 500,
    width = 5,
    height = 3
  )

})

# If you want to plot the power -----

# ggplot(
#   power_df,
#   aes(
#     x = overshoot_ratio,
#     y = power,
#     ymin = power_lb,
#     ymax = power_ub
#   )
# ) +
#   geom_ribbon(alpha = 0.25) +
#   geom_line(linewidth = 0.3) +
#   geom_point(size = 1) +
#   scale_y_continuous(
#     labels = scales::percent_format(accuracy = 1)
#   ) +
#   scale_x_continuous(breaks = seq(1.25, 3, by = 0.25)) +
#   labs(
#     x = "Excess power ratio",
#     y = "Anytime-valid power"
#   ) +
#   theme_minimal(base_size = 11) +
#   theme(
#     legend.position = "none",
#     panel.grid.minor = element_blank()
#   )