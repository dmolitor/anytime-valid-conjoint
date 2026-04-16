suppressPackageStartupMessages({
  library(ggplot2)
  library(ggtext)
  library(scales)
  library(viridis)
  library(here)
  library(readr)
})

significance_level <- 0.05

false_positives <- read_csv(here("data", "figure2.csv"), show_col_types = FALSE)

suppressWarnings({
  false_positive_plot <- ggplot(
      false_positives,
      aes(
        x = i,
        y = p_false_positive,
        ymin = p_false_positive_lower,
        ymax = p_false_positive_upper,
        color = Method,
        fill = Method
      )
    ) +
    geom_point(alpha = 0.3, size = 0.75) +
    geom_linerange(alpha = 0.3) +
    geom_line(linewidth = 0.3) +
    geom_hline(yintercept = significance_level, linetype = "dashed", color = "black") +
    labs(x = "Sample size", y = "**Pr(** False positive **)**", color = "", fill = "") +
    scale_color_viridis(discrete = 2, begin = 0.3, end = 0.7, option = "D") +
    scale_y_continuous(labels = label_percent()) +
    theme_minimal() +
    theme(axis.title.y = element_markdown(), legend.position = "bottom")

  ggsave(
    here("figures", "figure2.png"),
    plot = false_positive_plot,
    dpi = 500,
    width = 5,
    height = 4
  )
})