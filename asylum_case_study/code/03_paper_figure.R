suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(here)
  library(readr)
})

source(here("code", "cj.R"))
source(here("code", "figure_style.R"))

final_comparison <- read_csv(
  here("asylum_case_study", "data", "final_comparison.csv"),
  show_col_types = FALSE
)
sequential_estimates <- read_csv(
  here("asylum_case_study", "data", "sequential_av_estimates.csv"),
  show_col_types = FALSE
)
meta <- read_csv(
  here("asylum_case_study", "data", "meta.csv"),
  show_col_types = FALSE
)

attribute_display <- c(
  cconsist  = "Testimony consistency",
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

term_attribute <- function(term) {
  attribute_vars[vapply(attribute_vars, function(a) startsWith(term, a), logical(1))][1]
}

all_terms <- final_comparison |>
  distinct(term) |>
  pull(term)
term_attr_map <- vapply(all_terms, term_attribute, character(1))
term_level_map <- substring(all_terms, nchar(term_attr_map) + 1)
term_labels <- setNames(
  paste0(attribute_display[term_attr_map], ": ", term_level_map),
  all_terms
)

plot_data <- final_comparison |>
  mutate(
    label = factor(term_labels[term], levels = rev(term_labels[all_terms])),
    method = factor(method, levels = c("Conventional (fixed-n)", "Anytime-valid (CS)"))
  )

dir.create(here("figures"), showWarnings = FALSE)

asylum_plot <- ggplot(
  plot_data,
  aes(x = label, y = estimate, ymin = conf.low, ymax = conf.high, color = method)
) +
  geom_hline(yintercept = 0, linetype = "dotted", color = paper_colors$reference) +
  geom_pointrange(
    position = position_dodge(width = 0.55),
    size = 0.22,
    linewidth = 0.42
  ) +
  coord_flip() +
  scale_color_manual(values = method_palette) +
  labs(x = NULL, y = "AMCE estimate (95% interval)", color = NULL) +
  conjoint_theme(base_size = 9) +
  theme(
    axis.text.y = element_text(size = 7.2)
  )

save_paper_figure(
  "asylum_final_comparison.png",
  plot = asylum_plot,
  width = 8.2,
  height = 7.4
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

eprocess_plot <- ggplot(
  eprocess_data,
  aes(x = G, y = log_e_value)
) +
  geom_line(color = paper_colors$av, linewidth = 0.45) +
  geom_hline(yintercept = log(threshold), linetype = "dashed", color = paper_colors$accent) +
  geom_hline(yintercept = 0, linetype = "dotted", color = paper_colors$reference) +
  scale_x_continuous(labels = scales::label_comma()) +
  facet_wrap(~ label, ncol = 3, scales = "free_y") +
  labs(
    x = "Respondent clusters (G)",
    y = "Log e-value"
  ) +
  conjoint_theme(base_size = 9)

save_paper_figure(
  "asylum_eprocess_growth.png",
  plot = eprocess_plot,
  width = 8.2,
  height = 6.8
)
