suppressPackageStartupMessages({
  library(ggplot2)
  library(ggtext)
  library(grid)
  library(gridExtra)
  library(fst)
  library(glue)
  library(scales)
  library(viridis)
  library(patchwork)
  library(here)
  library(dplyr)
})

sample_efficiency_df <- suppressMessages({
  read_fst(here("data", "figure_3_6_efficiency.fst"))
}) |> as_tibble()

## Appendix Figure 3 ----------------------------------------------------------

suppressWarnings({

  # Plot mean sample size savings induced by early stopping

  early_stopping_sample_plot <- ggplot(
      sample_efficiency_df,
      aes(
        x = amce,
        y = p_sample_save,
        ymin = p_sample_save_lb,
        ymax = p_sample_save_ub,
        color = N
      )
    ) +
    geom_line(linewidth = 0.5, alpha = 0.3) +
    geom_linerange(alpha = 0.3) +
    geom_point(alpha = 0.3, size = 0.6) +
    geom_smooth(
      formula = y ~ x,
      method = "loess",
      se = FALSE,
      linewidth = 0.3,
      span = 0.4
    ) +
    facet_wrap(~ n_lev, ncol = 1) +
    scale_x_continuous(
      breaks = seq(0.02, 0.12, length.out = 6),
      labels = function(x) {
        x <- round(x, 3)
        x <- format(x, trim = TRUE, scientific = FALSE)
        sub("^(-?)0\\.", "\\1.", x)
      }
    ) +
    scale_y_continuous(labels = percent, limits = c(0, 1)) +
    labs(
      x = "AMCE",
      y = "Mean sample saved (%)",
      color = ""
    ) +
    theme_minimal() +
    scale_color_viridis(discrete = TRUE) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))

  # Plot probability of early stopping by AMCE (effect size)

  early_stopping_pr_plot <- ggplot(
    sample_efficiency_df,
    aes(
      x = amce,
      y = p_early,
      ymin = p_early_lb,
      ymax = p_early_ub,
      color = N
    )
  ) +
    geom_point(alpha = 0.3, size = 0.6) +
    geom_line(linewidth = 0.5, alpha = 0.3) +
    geom_linerange(alpha = 0.3) +
    geom_smooth(
      formula = y ~ x,
      method = "loess",
      se = FALSE,
      linewidth = 0.3,
      span = 0.4
    ) +
    facet_wrap(~ n_lev, ncol = 1) +
    scale_x_continuous(
      breaks = seq(0.02, 0.12, length.out = 6),
      labels = function(x) {
        x <- round(x, 3)
        x <- format(x, trim = TRUE, scientific = FALSE)
        sub("^(-?)0\\.", "\\1.", x)
      }
    ) +
    labs(
      x = "AMCE",
      y = "**Pr(** Early Stopping **)**"
    ) +
    theme_minimal() +
    scale_color_viridis(discrete = TRUE) +
    guides(color = "none") +
    theme(
      axis.title.y = element_markdown(),
      plot.title = element_text(hjust = 0.5, face = "bold")
    )

  sample_efficiency_plot <- early_stopping_pr_plot +
    early_stopping_sample_plot +
    plot_layout(ncol = 2, guides = "collect") &
    theme(legend.position = "bottom")

  # ggsave(
  #   here("figures", "figure6.png"),
  #   plot = sample_efficiency_plot,
  #   dpi = 500,
  #   width = 8,
  #   height = 8
  # )

})

## Figure 3 -------------------------------------------------------------------

# Representative point to annotate
annot_n <- 6000
annot_amce <- 0.05

# Pull the nearest observed point for each panel
annot_df <- sample_efficiency_df |>
  filter(n_lev == "Attribute levels: 6", N == glue("N: {annot_n}")) |>
  mutate(dist = abs(amce - annot_amce)) |>
  arrange(dist) |>
  slice(1)

# Values for labels
annot_p_early <- round(annot_df$p_early[[1]], 2)
annot_p_save <- round(annot_df$p_sample_save[[1]], 2)
annot_n_save <- signif(annot_n * annot_p_save, digits = 2)

annot_label_early <- glue(
  "At N = {comma(annot_n)} and AMCE = {number(annot_df$amce[[1]], accuracy = 0.01)},\n",
  "Pr(early stopping) = {percent(annot_p_early, accuracy = 1)}"
)

annot_label_save <- glue(
  "At N = {comma(annot_n)} and AMCE = {number(annot_df$amce[[1]], accuracy = 0.01)}, ",
  "mean\nsample saved = {percent(annot_p_save, accuracy = 1)} ",
  "(about N = {comma(annot_n_save)})"
)

suppressWarnings({
  sample_efficiency_sample_plot <- ggplot(
      sample_efficiency_df |> filter(n_lev == "Attribute levels: 6"),
      aes(
        x = amce,
        y = p_sample_save,
        ymin = p_sample_save_lb,
        ymax = p_sample_save_ub,
        color = N
      )
    ) +
    geom_line(linewidth = 0.5, alpha = 0.3) +
    geom_linerange(alpha = 0.3) +
    geom_point(alpha = 0.3, size = 0.6) +
    geom_smooth(
      formula = y ~ x,
      method = "loess",
      se = FALSE,
      linewidth = 0.3,
      span = 0.4
    ) +
    geom_curve(
      data = annot_df,
      aes(
        x = 0.063, y = 0.18,
        xend = amce, yend = p_sample_save
      ),
      inherit.aes = FALSE,
      curvature = -0.25,
      arrow = arrow(length = unit(0.015, "npc")),
      linewidth = 0.4,
      color = "black"
    ) +
    annotate(
      "text",
      x = 0.0632,
      y = 0.16,
      label = annot_label_save,
      hjust = 0,
      vjust = 1,
      size = 3
    ) +
    scale_x_continuous(
      breaks = seq(0.02, 0.12, length.out = 6),
      labels = function(x) {
        x <- round(x, 3)
        x <- format(x, trim = TRUE, scientific = FALSE)
        sub("^(-?)0\\.", "\\1.", x)
      }
    ) +
    scale_y_continuous(labels = percent, limits = c(0, 1)) +
    labs(
      x = "AMCE",
      y = "Mean sample saved (%)",
      color = ""
    ) +
    theme_minimal() +
    scale_color_viridis(discrete = TRUE) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))

  sample_efficiency_pr_plot <- ggplot(
    sample_efficiency_df |> filter(n_lev == "Attribute levels: 6"),
    aes(
      x = amce,
      y = p_early,
      ymin = p_early_lb,
      ymax = p_early_ub,
      color = N
    )
  ) +
    geom_point(alpha = 0.3, size = 0.6) +
    geom_line(linewidth = 0.5, alpha = 0.3) +
    geom_linerange(alpha = 0.3) +
    geom_smooth(
      formula = y ~ x,
      method = "loess",
      se = FALSE,
      linewidth = 0.3,
      span = 0.4
    ) +
    geom_curve(
      data = annot_df,
      aes(
        x = 0.065, y = 0.30,
        xend = amce, yend = p_early
      ),
      inherit.aes = FALSE,
      curvature = -0.25,
      arrow = arrow(length = unit(0.015, "npc")),
      linewidth = 0.4,
      color = "black"
    ) +
    annotate(
      "text",
      x = 0.067,
      y = 0.28,
      label = annot_label_early,
      hjust = 0,
      vjust = 1,
      size = 3
    ) +
    scale_x_continuous(
      breaks = seq(0.02, 0.12, length.out = 6),
      labels = function(x) {
        x <- round(x, 3)
        x <- format(x, trim = TRUE, scientific = FALSE)
        sub("^(-?)0\\.", "\\1.", x)
      }
    ) +
    labs(
      x = "AMCE",
      y = "**Pr(** Early Stopping **)**"
    ) +
    theme_minimal() +
    scale_color_viridis(discrete = TRUE) +
    guides(color = "none") +
    theme(
      axis.title.y = element_markdown(),
      plot.title = element_text(hjust = 0.5, face = "bold")
    )

  sample_efficiency_plot <- sample_efficiency_pr_plot +
    sample_efficiency_sample_plot +
    plot_layout(ncol = 2, guides = "collect") &
    theme(legend.position = "bottom")

  # ggsave(
  #   here("figures", "figure3.png"),
  #   plot = sample_efficiency_plot,
  #   dpi = 500,
  #   width = 9,
  #   height = 4
  # )
})

## This data compares power curves between anytime-valid and fixed-n methods
## Not currently included in the paper.

# sim_efficiency_df <- read_fst(here("data", "figure_3_6_av.fst"))
# sim_efficiency_fixed_df <- read_fst(here("data", "figure_3_6_fixed.fst"))

# # Anytime-valid power curve data

# power_curves_av <- sim_efficiency_df |>
#   mutate(stat_sig = 0 < conf.low | 0 > conf.high) |>
#   group_by(n_lev, attribute, level, sim_iter, amce, N) |>
#   summarize(type2_error = all(!stat_sig), .groups = "drop_last") |>
#   ungroup() |>
#   group_by(n_lev, attribute, level, amce, N) |>
#   summarize(
#     power = mean(!type2_error),
#     power_se = sd(!type2_error)/sqrt(n()),
#     power_lb = power - 1.96*power_se,
#     power_ub = power + 1.96*power_se,
#     .groups = "drop_last"
#   ) |>
#   ungroup()

# # Fixed-n power curve data

# power_curves_fixed <- sim_efficiency_fixed_df |>
#   mutate(stat_sig = 0 < conf.low | 0 > conf.high) |>
#   group_by(n_lev, attribute, level, sim_iter, amce, N) |>
#   summarize(type2_error = all(!stat_sig), .groups = "drop_last") |>
#   ungroup() |>
#   group_by(n_lev, attribute, level, amce, N) |>
#   summarize(
#     power = mean(!type2_error),
#     power_se = sd(!type2_error)/sqrt(n()),
#     power_lb = power - 1.96*power_se,
#     power_ub = power + 1.96*power_se,
#     .groups = "drop_last"
#   ) |>
#   ungroup()

# power_curves_df <- filter(power_curves_av, attribute == "Region", amce <= 0.12001) |>
#   mutate(which = "Anytime-valid") |>
#   bind_rows(
#     filter(power_curves_fixed, attribute == "Region", amce <= 0.12001) |>
#       mutate(which = "Fixed-N")
#   ) |>
#   mutate(
#     n_eff = factor(
#       paste("N:", N),
#       levels = paste("N:", unique(power_curves_av$N))
#     ),
#     n_lev = factor(paste("Attribute levels:", n_lev))
#   )

# power_curves_plot <- power_curves_df |>
#   ggplot(
#     aes(
#       x = amce,
#       y = power,
#       ymin = power_lb,
#       ymax = power_ub,
#       color = which
#     )
#   ) +
#   geom_line(linewidth = 0.5, alpha = 0.3) +
#   geom_linerange(alpha = 0.3) +
#   geom_smooth(
#     formula = y ~ x,
#     method = "loess",
#     span = 0.25,
#     se = FALSE,
#     linewidth = 0.5
#   ) +
#   facet_wrap(~ n_lev + n_eff) +
#   theme_minimal() +
#   theme(
#     panel.grid.minor = element_blank(),
#     plot.title = element_text(hjust = 0.5, face = "bold")
#   ) +
#   scale_x_continuous(
#     breaks = seq(0.02, 0.12, length.out = 6),
#     labels = function(x) {
#       x <- round(x, 3)
#       x <- format(x, trim = TRUE, scientific = FALSE)
#       sub("^(-?)0\\.", "\\1.", x)
#     }
#   ) +
#   scale_y_continuous(labels = percent) +
#   labs(
#     x = "AMCE",
#     y = "Statistical power",
#     color = "Method"
#   )

# ggsave(
#   here("figures", "power_curve_comparison.png"),
#   dpi = 500,
#   plot = power_curves_plot,
#   width = 9,
#   height = 6
# )