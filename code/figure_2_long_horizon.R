suppressPackageStartupMessages({
  library(ggplot2)
  library(here)
  library(readr)
})

source(here("code", "figure_style.R"))

alpha <- 0.05
results <- read_csv(
  here("data", "figure2_long_horizon.csv"),
  show_col_types = FALSE
)

p <- ggplot(
  results,
  aes(x = G, y = p_false_positive, color = Method, fill = Method)
) +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high), alpha = 0.12, color = NA) +
  geom_line(linewidth = 0.6) +
  geom_hline(
    yintercept = alpha,
    linetype = "dashed",
    color = paper_colors$reference
  ) +
  scale_x_log10(
    labels = scales::label_comma(),
    expand = expansion(mult = c(0.03, 0.08))
  ) +
  scale_y_continuous(
    labels = scales::label_percent(accuracy = 1),
    limits = c(0, NA)
  ) +
  scale_color_manual(values = method_palette) +
  scale_fill_manual(values = method_palette) +
  facet_wrap(~ Test) +
  labs(
    x = "Respondent clusters (G, log scale)",
    y = "Cumulative Type I error",
    color = NULL,
    fill = NULL
  ) +
  conjoint_theme()

save_paper_figure(
  "figure2_long_horizon.png",
  plot = p,
  width = 8,
  height = 4.8
)
