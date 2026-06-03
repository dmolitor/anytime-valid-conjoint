suppressPackageStartupMessages({
  library(ggplot2)
  library(here)
  library(fst)
  library(scales)
  library(tibble)
})

power_df <- suppressMessages({
  read_fst(here("data", "figure4.fst"))
}) |> as_tibble()

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
      labels = scales::percent_format(accuracy = 1),
      breaks = seq(0.2, 0.8, by = 0.1)
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