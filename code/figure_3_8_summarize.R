# Summarize the anytime-valid sample-efficiency simulations (Figures 3 and 8).
#
# Reads the per-replication interim results written by
# code/figure_3_8_simulations.R (data/figure_3_8_av.fst) and writes the
# cell-level summary consumed by code/figure_3_8.R (data/figure_3_8.fst).
# figure_3_8_simulations.R sources this file at the end of a run; it can also be
# run on its own, without re-running the simulations, whenever the
# per-replication file is available:
#
#   Rscript code/figure_3_8_summarize.R
#
# Units. In cj.R the interim index `i` counts respondents (clusters): one
# interim analysis every `chunk_size` respondents, up to `experiment_size`
# respondents. The cap `N` recorded by figure_3_8_simulations.R is the effective
# sample size N_max = respondents x tasks. Both are put on the same (effective)
# scale below before a stopping time is compared with, or divided by, N_max.
# An earlier version compared `i` (respondents) directly with `N` (effective
# units), which credited a run stopping at respondent i with savings of
# 1 - i/N_max instead of 1 - i/G_max, and counted a first crossing at the final
# interim analysis as an early stop.

suppressPackageStartupMessages({
  library(dplyr)
  library(here)
  library(fst)
})

# Must match the value used in figure_3_8_simulations.R.
if (!exists("tasks_per_respondent")) tasks_per_respondent <- 2

sim_efficiency_df <- read_fst(here("data", "figure_3_8_av.fst"))

sample_efficiency_df <- sim_efficiency_df |>
  filter(attribute == "Region", amce <= 0.12001) |> # set to 0.12001 for floating point inclusion
  mutate(stat_sig = 0 < conf.low | 0 > conf.high) |>
  group_by(n_lev, attribute, level, sim_iter, amce, N) |>
  summarize(
    # Stopping time in effective-sample-size units (respondents x tasks), i.e.
    # the units of N. A run that never stops is assigned N (no savings).
    early_stop = if (any(stat_sig)) {
      i[min(which(stat_sig))] * tasks_per_respondent
    } else {
      first(N)
    },
    N_effective = first(N),
    .groups = "drop"
  ) |>
  ungroup() |>
  group_by(n_lev, attribute, level, amce, N) |>
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
    p_sample_save_se = sd(1 - early_stop/N_effective)/sqrt(n()),
    p_sample_save_lb = p_sample_save - 1.96*p_sample_save_se,
    p_sample_save_ub = p_sample_save + 1.96*p_sample_save_se,
    .groups = "drop_last"
  ) |>
  ungroup() |>
  mutate(n_lev = factor(paste("Attribute levels:", n_lev)))

suppressMessages({
  write_fst(sample_efficiency_df, here("data", "figure_3_8.fst"))
})
