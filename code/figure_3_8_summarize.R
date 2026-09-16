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
# respondents, and every interim analysis is recorded whether or not the
# confidence sequence has excluded zero. The respondent horizon of a run is
# therefore G_max = max(i), and all stopping-time quantities below are computed
# in respondents against G_max. The cap `N` stored by figure_3_8_simulations.R
# (the effective sample size, respondents x tasks, since commit 0f32496; the
# respondent count before it) is carried through for labelling only, so the
# summary is correct for per-replication files produced by either version.
# An earlier summary compared `i` (respondents) directly with `N` (effective
# units), which credited a run stopping at respondent i with savings of
# 1 - i/N instead of 1 - i/G_max, and counted a first crossing at the final
# interim analysis as an early stop.

suppressPackageStartupMessages({
  library(dplyr)
  library(here)
  library(fst)
})

sim_efficiency_df <- read_fst(here("data", "figure_3_8_av.fst"))

sample_efficiency_df <- sim_efficiency_df |>
  filter(attribute == "Region", amce <= 0.12001) |> # set to 0.12001 for floating point inclusion
  mutate(stat_sig = 0 < conf.low | 0 > conf.high) |>
  group_by(n_lev, attribute, level, sim_iter, amce, N) |>
  summarize(
    # Respondent horizon of the run and stopping time in respondents. A run
    # whose confidence sequence never excludes zero is assigned G_max (no
    # savings); a first crossing at the final look is not an early stop.
    G_max = max(i),
    early_stop = if (any(stat_sig)) {
      i[min(which(stat_sig))]
    } else {
      max(i)
    },
    .groups = "drop"
  ) |>
  ungroup() |>
  group_by(n_lev, attribute, level, amce, N) |>
  summarize(
    G_max = first(G_max),
    median_stop = median(early_stop),
    mean_stop = mean(early_stop),
    mean_stop_se = sd(early_stop)/sqrt(n()),
    mean_stop_lb = mean_stop - 1.96*mean_stop_se,
    mean_stop_ub = mean_stop + 1.96*mean_stop_se,
    p_early = mean(early_stop < G_max),
    p_early_se = sd(early_stop < G_max)/sqrt(n()),
    p_early_lb = p_early - 1.96*p_early_se,
    p_early_ub = p_early + 1.96*p_early_se,
    p_sample_save = mean(1 - early_stop/G_max),
    p_sample_save_se = sd(1 - early_stop/G_max)/sqrt(n()),
    p_sample_save_lb = p_sample_save - 1.96*p_sample_save_se,
    p_sample_save_ub = p_sample_save + 1.96*p_sample_save_se,
    .groups = "drop_last"
  ) |>
  ungroup() |>
  mutate(n_lev = factor(paste("Attribute levels:", n_lev)))

suppressMessages({
  write_fst(sample_efficiency_df, here("data", "figure_3_8.fst"))
})
