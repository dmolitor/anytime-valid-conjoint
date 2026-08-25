suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(here)
  library(readr)
  library(scales)
})

source(here("code", "figure_style.R"))

results <- read_csv(here("data", "figure7.csv")) |>
  mutate(
    Test = case_when(
      Test == "Multivariate region null" ~ "Multivariate null",
      TRUE ~ "Scalar null"
    )
  )

p <- ggplot(results, aes(x = G, y = p_false_positive, color = Method, fill = Method)) +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high), alpha = 0.3, color = NA) +
  geom_line() +
  geom_hline(yintercept = alpha, linetype = "dashed", color = paper_colors$reference) +
  scale_x_log10(labels = label_comma()) +
  scale_y_continuous(labels = label_percent(accuracy = 1), limits = c(0, NA)) +
  scale_color_manual(values = method_palette) +
  scale_fill_manual(values = method_palette) +
  facet_wrap(~ Test) +
  labs(
    x = "Number of respondents (G, log scale)",
    y = "Cumulative Type I error",
    color = NULL,
    fill = NULL
  ) +
  conjoint_theme()

save_paper_figure("figure7.png", p, width = 6, height = 4, dpi = 500)
