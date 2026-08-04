suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(scales)
  library(here)
  library(readr)
})

source(here("code", "figure_style.R"))

significance_level <- 0.05

false_positives <- read_csv(here("data", "figure2.csv"), show_col_types = FALSE)
false_positives <- false_positives |>
  mutate(
    Test = factor(
      Test,
      levels = c("Single region coefficient", "Joint region coefficients"),
      labels = c("Scalar region null", "Multivariate region null")
    )
  )

suppressWarnings({
  false_positive_plot <- ggplot(
      false_positives,
      aes(
        x = i,
        y = p_false_positive,
        ymin = p_false_positive_lower,
        ymax = p_false_positive_upper,
        color = Method
      )
    ) +
    geom_ribbon(aes(fill = Method), alpha = 0.12, color = NA) +
    geom_line(linewidth = 0.6) +
    geom_hline(yintercept = significance_level, linetype = "dashed", color = paper_colors$reference) +
    facet_wrap(~ Test, ncol = 2) +
    labs(
      x = "Respondent clusters (G)",
      y = "Cumulative Type I error",
      color = NULL,
      fill = NULL
    ) +
    scale_color_manual(values = method_palette) +
    scale_fill_manual(values = method_palette) +
    scale_y_continuous(labels = label_percent(accuracy = 1)) +
    conjoint_theme()

  save_paper_figure(
    "figure2.png",
    plot = false_positive_plot,
    width = 8,
    height = 4.8
  )
})
