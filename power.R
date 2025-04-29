library(dplyr)
library(ggplot2)
library(ggtext)
library(grid)
library(gridExtra)
library(here)
library(readr)
library(scales)
library(viridis)

source(here("src/anytime_valid_conjoint/simulation.R"))
source(here("src/anytime_valid_conjoint/utils.R"))

options(future.globals.maxSize = Inf)

## Run anytime-valid power calculation simulations; 4 levels ------------------

sim_power <- lapply(
  seq(1500, 9000, by = 1500),
  function(sim_n) {
    lapply(
      seq(0.02, 0.1, by = 0.01),
      function(amce) {
        amce_grid <- seq(amce, amce + 0.0075, length.out = 4)
        cat("Sample size:", sim_n, "\n")
        cat("AMCE grid:", paste0(amce_grid, collapse = ", "), "\n")
        amce_grid_df <- tribble(
          ~term, ~contrast, ~estimate_truth,
          "region", "Low", amce_grid[[1]],
          "region", "Medium", amce_grid[[2]],
          "region", "High", amce_grid[[3]],
          "region", "Extreme", amce_grid[[4]]
        )
        conjoint_sim_power <- retry(
          power(
            levels = list(region = c("None", "Low", "Medium", "High", "Extreme")),
            amce_fn = !!expr(
              !!amce_grid[[1]] * (region == "Low") +
              !!amce_grid[[2]] * (region == "Medium") +
              !!amce_grid[[3]] * (region == "High") +
              !!amce_grid[[4]] * (region == "Extreme")
            ),
            formula = outcome ~ region,
            n = sim_n,
            skip = 30,
            n_sim = 500,
            parallel = TRUE,
            verbose = TRUE
          )
        )
        conjoint_sim_power <- conjoint_sim_power |>
          left_join(
            amce_grid_df,
            by = c("term", "contrast")
          ) |>
          mutate(N = sim_n)
        return(conjoint_sim_power)
      }
    )
  }
)
sim_power_df <- bind_rows(sim_power)
write_csv(
  sim_power_df,
  here("data", "power_curves_av_lev4.csv")
)

## Run anytime-valid power calculation simulations; 5 levels ------------------

sim_power_lev5 <- lapply(
  seq(1500, 9000, by = 1500),
  function(sim_n) {
    lapply(
      seq(0.02, 0.1, by = 0.01),
      function(amce) {
        amce_grid <- seq(amce, amce + 0.01, length.out = 6)[1:5]
        cat("Sample size:", sim_n, "\n")
        cat("AMCE grid:", paste0(amce_grid, collapse = ", "), "\n")
        amce_grid_df <- tribble(
          ~term, ~contrast, ~estimate_truth,
          "region", "Low", amce_grid[[1]],
          "region", "Medium", amce_grid[[2]],
          "region", "High", amce_grid[[3]],
          "region", "Extreme", amce_grid[[4]],
          "region", "Extreme+", amce_grid[[5]],
        )
        conjoint_sim_power <- retry(
          power(
            levels = list(region = c("None", "Low", "Medium", "High", "Extreme", "Extreme+")),
            amce_fn = !!expr(
              !!amce_grid[[1]] * (region == "Low") +
              !!amce_grid[[2]] * (region == "Medium") +
              !!amce_grid[[3]] * (region == "High") +
              !!amce_grid[[4]] * (region == "Extreme") +
              !!amce_grid[[5]] * (region == "Extreme+")
            ),
            formula = outcome ~ region,
            n = sim_n,
            skip = 30,
            n_sim = 500,
            parallel = TRUE,
            verbose = TRUE
          ),
          n = 10
        )
        conjoint_sim_power <- conjoint_sim_power |>
          left_join(
            amce_grid_df,
            by = c("term", "contrast")
          ) |>
          mutate(N = sim_n)
        return(conjoint_sim_power)
      }
    )
  }
)
sim_power_df_lev5 <- bind_rows(sim_power_lev5)
write_csv(
  sim_power_df_lev5,
  here("data", "power_curves_av_lev5.csv")
)

## Run anytime-valid power calculation simulations; 6 levels ------------------

sim_power_lev6 <- lapply(
  seq(1500, 9000, by = 1500),
  function(sim_n) {
    lapply(
      seq(0.02, 0.1, by = 0.01),
      function(amce) {
        amce_grid <- seq(amce, amce + 0.01, length.out = 7)[1:6]
        cat("Sample size:", sim_n, "\n")
        cat("AMCE grid:", paste0(amce_grid, collapse = ", "), "\n")
        amce_grid_df <- tribble(
          ~term, ~contrast, ~estimate_truth,
          "region", "Low", amce_grid[[1]],
          "region", "Medium", amce_grid[[2]],
          "region", "High", amce_grid[[3]],
          "region", "Extreme", amce_grid[[4]],
          "region", "Extreme+", amce_grid[[5]],
          "region", "Extreme++", amce_grid[[6]]
        )
        conjoint_sim_power <- retry(
          power(
            levels = list(region = c("None", "Low", "Medium", "High", "Extreme", "Extreme+", "Extreme++")),
            amce_fn = !!expr(
              !!amce_grid[[1]] * (region == "Low") +
              !!amce_grid[[2]] * (region == "Medium") +
              !!amce_grid[[3]] * (region == "High") +
              !!amce_grid[[4]] * (region == "Extreme") +
              !!amce_grid[[5]] * (region == "Extreme+") +
              !!amce_grid[[6]] * (region == "Extreme++")
            ),
            formula = outcome ~ region,
            n = sim_n,
            skip = 30,
            n_sim = 500,
            parallel = TRUE,
            verbose = TRUE
          ), 
          n = 10
        )
        conjoint_sim_power <- conjoint_sim_power |>
          left_join(
            amce_grid_df,
            by = c("term", "contrast")
          ) |>
          mutate(N = sim_n)
        return(conjoint_sim_power)
      }
    )
  }
)
sim_power_df_lev6 <- bind_rows(sim_power_lev6)
write_csv(
  sim_power_df_lev6,
  here("data", "power_curves_av_lev6.csv")
)

# Simulate power for standard fixed-n regression estimates --------------------

sim_power_fixed <- lapply(
  seq(1500, 9000, by = 1500),
  function(sim_n) {
    lapply(
      seq(0.02, 0.1, by = 0.01),
      function(amce) {
        amce_grid <- seq(amce, amce + 0.0075, by = 0.0025)
        cat("Sample size:", sim_n, "\n")
        cat("AMCE grid:", paste0(amce_grid, collapse = ", "), "\n")
        amce_grid_df <- tribble(
          ~term, ~contrast, ~estimate_truth,
          "region", "Low", amce_grid[[1]],
          "region", "Medium", amce_grid[[2]],
          "region", "High", amce_grid[[3]],
          "region", "Extreme", amce_grid[[4]]
        )
        conjoint_sim_power_fixed <- retry(
          power_fixed(
            levels = list(region = c("None", "Low", "Medium", "High", "Extreme")),
            amce_fn = !!expr(
              !!amce_grid[[1]] * (region == "Low") +
              !!amce_grid[[2]] * (region == "Medium") +
              !!amce_grid[[3]] * (region == "High") +
              !!amce_grid[[4]] * (region == "Extreme")
            ),
            formula = outcome ~ region,
            n = sim_n,
            n_sim = 500,
            parallel = TRUE,
            verbose = TRUE
          )
        )
        conjoint_sim_power_fixed <- conjoint_sim_power_fixed |>
          left_join(
            amce_grid_df,
            by = c("term", "contrast")
          ) |>
          mutate(N = sim_n)
        return(conjoint_sim_power_fixed)
      }
    )
  }
)
sim_power_fixed_df <- bind_rows(sim_power_fixed)
write_csv(
  sim_power_fixed_df,
  here("data", "power_curves_fixed.csv")
)


# Calculate the power of both methods -----------------------------------------

## Anytime-valid power curve data

power_curves_av <- sim_power_df |>
  mutate(stat_sig = 0 < cs_lower | 0 > cs_upper) |>
  group_by(term, contrast, reference, sim_iter, estimate_truth, N) |>
  summarize(type2_error = all(!stat_sig), .groups = "drop_last") |>
  ungroup() |>
  group_by(term, contrast, reference, estimate_truth, N) |>
  summarize(
    power = mean(!type2_error),
    power_se = sd(!type2_error)/sqrt(n()),
    power_lb = power - 1.96*power_se,
    power_ub = power + 1.96*power_se,
    .groups = "drop_last"
  ) |>
  ungroup() |>
  filter(contrast != "None")

write_csv(
  power_curves_av,
  here("data", "power_curves_av_plot.csv")
)

## Fixed-n power curve data

power_curves_fixed <- sim_power_fixed_df |>
  mutate(stat_sig = 0 < cs_lower | 0 > cs_upper) |>
  group_by(term, contrast, reference, sim_iter, estimate_truth, N) |>
  summarize(type2_error = all(!stat_sig), .groups = "drop_last") |>
  ungroup() |>
  group_by(term, contrast, reference, estimate_truth, N) |>
  summarize(
    power = mean(!type2_error),
    power_se = sd(!type2_error)/sqrt(n()),
    power_lb = power - 1.96*power_se,
    power_ub = power + 1.96*power_se,
    .groups = "drop_last"
  ) |>
  ungroup() |>
  filter(contrast != "None")

write_csv(
  power_curves_fixed,
  here::here("data", "power_curves_fixed_plot.csv")
)

# Plot power curves -----------------------------------------------------------

power_curves_plot <- mutate(power_curves_av, which = "Anytime-valid") |>
  bind_rows(mutate(power_curves_fixed, which = "Fixed-n")) |>
  mutate(n_eff = factor(N*2, levels = seq(1500, 9000, by = 1500)*2)) |>
  ggplot(
    aes(
      x = estimate_truth,
      y = power,
      ymin = power_lb,
      ymax = power_ub,
      color = which
    )
  ) +
  geom_line(linewidth = 0.5, alpha = 0.3) +
  geom_linerange(alpha = 0.3) +
  geom_smooth(
    formula = y ~ x,
    method = "loess",
    span = 0.25,
    se = FALSE,
    linewidth = 0.5
  ) +
  facet_wrap(~ n_eff, labeller = as_labeller(\(x) paste("N:", x))) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold")
  ) +
  scale_x_continuous(
    breaks = seq(0.02, 0.1075, length.out = 6),
    labels = function(x) {
      x <- round(x, 3)
      x <- format(x, trim = TRUE, scientific = FALSE)
      sub("^(-?)0\\.", "\\1.", x)
    }
  ) +
  scale_y_continuous(labels = percent) +
  labs(
    x = "AMCE",
    y = "Statistical power",
    color = "Method"
  )
power_curves_plot <- ggdraw() +
  draw_plot(power_curves_plot) +
  draw_label(
    "Levels: 4",
    x      = 0.78,    # 50% from left
    y      = 0.58,    # 50% from bottom
    hjust  = 0.5,    # center horizontally
    vjust  = 0.5,    # center vertically
    size   = 10,
    fontface = "bold"
  )

ggsave(
  here("figures", "power_curve_comparison.png"),
  plot = power_curves_plot,
  width = 9,
  height = 6
)
# Save it with coords flipped
# ggsave(
#   here("figures", "power_curve_comparison_flipped.png"),
#   plot = power_curves_plot + coord_flip(),
#   width = 9,
#   height = 6
# )

# Plot stopping times for each AMCE -------------------------------------------

early_stopping <- sim_power_df |>
  mutate(stat_sig = 0 < cs_lower | 0 > cs_upper) |>
  group_by(term, contrast, reference, sim_iter, estimate_truth, N) |>
  summarize(
    early_stop = if (any(stat_sig)) {
      iter[min(which(stat_sig))]*2
    } else {
      first(N)*2
    },
    N_effective = first(N)*2, # 1 respondent x 1 task x 2 profiles = 2
    .groups = "drop"
  ) |>
  ungroup() |>
  group_by(term, contrast, reference, estimate_truth, N_effective) |>
  summarize(
    median_stop = median(early_stop),
    mean_stop = mean(early_stop),
    mean_stop_se = sd(early_stop)/sqrt(n()),
    mean_stop_lb = mean_stop - 1.96*mean_stop_se,
    mean_stop_ub = mean_stop + 1.96*mean_stop_se,
    p_early = mean(early_stop < N_effective),
    p_early_se = sd(early_stop < N_effective)/sqrt(n()),
    p_early_lb = p_early - 1.96*p_early_se,
    p_early_ub = p_early + 1.96*p_early_se,
    p_sample_save = mean(1 - early_stop/N_effective),
    .groups = "drop_last"
  ) |>
  ungroup() |>
  filter(contrast != "None")

early_stopping_lev5 <- sim_power_df_lev5 |>
  mutate(stat_sig = 0 < cs_lower | 0 > cs_upper) |>
  group_by(term, contrast, reference, sim_iter, estimate_truth, N) |>
  summarize(
    early_stop = if (any(stat_sig)) {
      iter[min(which(stat_sig))]*2
    } else {
      first(N)*2
    },
    N_effective = first(N)*2, # 1 respondent x 1 task x 2 profiles = 2
    .groups = "drop"
  ) |>
  ungroup() |>
  group_by(term, contrast, reference, estimate_truth, N_effective) |>
  summarize(
    median_stop = median(early_stop),
    mean_stop = mean(early_stop),
    mean_stop_se = sd(early_stop)/sqrt(n()),
    mean_stop_lb = mean_stop - 1.96*mean_stop_se,
    mean_stop_ub = mean_stop + 1.96*mean_stop_se,
    p_early = mean(early_stop < N_effective),
    p_early_se = sd(early_stop < N_effective)/sqrt(n()),
    p_early_lb = p_early - 1.96*p_early_se,
    p_early_ub = p_early + 1.96*p_early_se,
    p_sample_save = mean(1 - early_stop/N_effective),
    .groups = "drop_last"
  ) |>
  ungroup() |>
  filter(contrast != "None")

early_stopping_lev6 <- sim_power_df_lev6 |>
  mutate(stat_sig = 0 < cs_lower | 0 > cs_upper) |>
  group_by(term, contrast, reference, sim_iter, estimate_truth, N) |>
  summarize(
    early_stop = if (any(stat_sig)) {
      iter[min(which(stat_sig))]*2
    } else {
      first(N)*2
    },
    N_effective = first(N)*2, # 1 respondent x 1 task x 2 profiles = 2
    .groups = "drop"
  ) |>
  ungroup() |>
  group_by(term, contrast, reference, estimate_truth, N_effective) |>
  summarize(
    median_stop = median(early_stop),
    mean_stop = mean(early_stop),
    mean_stop_se = sd(early_stop)/sqrt(n()),
    mean_stop_lb = mean_stop - 1.96*mean_stop_se,
    mean_stop_ub = mean_stop + 1.96*mean_stop_se,
    p_early = mean(early_stop < N_effective),
    p_early_se = sd(early_stop < N_effective)/sqrt(n()),
    p_early_lb = p_early - 1.96*p_early_se,
    p_early_ub = p_early + 1.96*p_early_se,
    p_sample_save = mean(1 - early_stop/N_effective),
    .groups = "drop_last"
  ) |>
  ungroup() |>
  filter(contrast != "None")

early_stopping_all <- bind_rows(
  mutate(early_stopping, n_levels = 4),
  mutate(early_stopping_lev5, n_levels = 5),
  mutate(early_stopping_lev6, n_levels = 6)
)

## Plot mean sample size savings induced by early stopping

# early_stopping_sample_plot <- ggplot(
#   early_stopping,
#   aes(
#     x = estimate_truth,
#     y = mean_stop,
#     ymin = mean_stop_lb,
#     ymax = mean_stop_ub
#   )
# ) +
#   geom_smooth(
#     formula = y ~ x,
#     method = "loess",
#     span = 0.25,
#     se = FALSE,
#     linewidth = 0.5,
#     color = "gray50",
#   ) +
#   geom_point(aes(color = p_early), size = 1.5) +
#   facet_wrap(
#     ~ N_effective,
#     scales = "free_y",
#     labeller = as_labeller(\(x) paste("N:", x))
#   ) +
#   scale_x_continuous(
#     breaks = seq(0.02, 0.1075, length.out = 6),
#     labels = function(x) {
#       x <- round(x, 3)
#       x <- format(x, trim = TRUE, scientific = FALSE)
#       sub("^(-?)0\\.", "\\1.", x)
#     }
#   ) +
#   scale_size_continuous(name = "P(early stop)") +
#   scale_color_viridis(name = "Pr(early stop)", option = "D") +
#   labs(
#     x = "AMCE",
#     y = "Mean stopping sample (N)",
#     title = "Stopping-time by AMCE and Sample Size"
#   ) +
#   theme_minimal() +
#   theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# ggsave(
#   here("figures", "early_stopping_times.png"),
#   plot = early_stopping_sample_plot,
#   width = 8,
#   height = 5
# )

early_stopping_sample_plot <- ggplot(
  early_stopping_all,
  aes(
    x = estimate_truth,
    y = p_sample_save,
    color = factor(n_levels)
  )
) +
  geom_point(alpha = 0.3, size = 0.6) +
  geom_smooth(
    formula = y ~ x,
    method = "loess",
    se = FALSE,
    linewidth = 0.5,
    span = 0.4
  ) +
  facet_wrap(
    ~ N_effective,
    labeller = as_labeller(\(x) paste("N:", x))
  ) +
  scale_x_continuous(
    breaks = seq(0.02, 0.1075, length.out = 6),
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
    color = "Levels"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave(
  here("figures", "early_stopping_sample_savings.png"),
  plot = early_stopping_sample_plot,
  width = 8,
  height = 5
)

## Plot probability of early stopping by AMCE (effect size)

early_stopping_pr_plot <- ggplot(
  early_stopping_all,
  aes(
    x = estimate_truth,
    y = p_early,
    color = factor(n_levels)
  )
) +
  geom_point(alpha = 0.3, size = 0.6) +
  geom_smooth(
    formula = y ~ x,
    method = "loess",
    se = FALSE,
    linewidth = 0.5,
    span = 0.4
  ) +
  facet_wrap(
    ~ N_effective,
    labeller = as_labeller(\(x) paste("N:", x))
  ) +
  scale_x_continuous(
    breaks = seq(0.02, 0.1075, length.out = 6),
    labels = function(x) {
      x <- round(x, 3)
      x <- format(x, trim = TRUE, scientific = FALSE)
      sub("^(-?)0\\.", "\\1.", x)
    }
  ) +
  labs(
    x = "AMCE",
    y = "**Pr(** Early Stopping **)**",
    color = "Levels"
  ) +
  theme_minimal() +
  theme(
    axis.title.y = element_markdown(),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

ggsave(
  here("figures", "early_stopping_pr.png"),
  plot = early_stopping_pr_plot,
  width = 7,
  height = 4
)

# Alternate version of the plot above

# early_stopping_pr_plot_alt <- ggplot(
#   early_stopping_all,
#   aes(
#     x = estimate_truth,
#     y = p_early,
#     ymin = p_early_lb,
#     ymax = p_early_ub,
#     color = factor(N*2)
#   )
# ) +
#   geom_point(alpha = 0.3, size = 0.6) +
#   geom_smooth(
#     formula = y ~ x,
#     method = "loess",
#     se = FALSE,
#     span = 0.4,
#     linewidth = 0.5
#   ) +
#   facet_wrap(
#     ~ n_levels,
#     labeller = as_labeller(\(x) paste("Levels:", x)),
#     nrow = 1
#   ) +
#   scale_x_continuous(
#     breaks = seq(0.02, 0.1075, length.out = 6),
#     labels = function(x) {
#       x <- round(x, 3)
#       x <- format(x, trim = TRUE, scientific = FALSE)
#       sub("^(-?)0\\.", "\\1.", x)
#     }
#   ) +
#   labs(
#     x = "AMCE",
#     y = "**Pr(** Early Stopping **)**",
#     color = "N"
#   ) +
#   theme_minimal() +
#   theme(
#     axis.title.y = element_markdown(),
#     plot.title = element_text(hjust = 0.5, face = "bold")
#   )

# ggsave(
#   here("figures", "early_stopping_pr_alt.png"),
#   plot = early_stopping_pr_plot_alt,
#   width = 8,
#   height = 4
# )
