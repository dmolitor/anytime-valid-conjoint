suppressPackageStartupMessages({
  library(here)
  library(cjpowR)
  library(dplyr)
  library(fst)
})

source(here("code", "cj.R"))

options(
  future.globals.maxSize = Inf,
  progressr.enable = TRUE
)

# Specify conjoint parameters
significance_level <- 0.05
target_power <- 0.8
tasks_per_respondent <- 1
target_levels <- 6
number_of_simulations <- 1000

# amce_grid <- seq(0.01, 0.05, length.out = 9); # Run this if you want a full grid of target AMCE values
amce_grid <- 0.03
ratio_grid <- c(1, 1.25, 1.5, 2, 2.5, 3)

## Calculate the empirical performance across a grid of AMCE values -----------

set.seed(641423)
power <- lapply(
  amce_grid,
  function(amce) {
    true_amces <- ratio_grid*amce
    regions <- setNames(true_amces, paste("Region", 1:length(true_amces)))
    amces <- list(
      Party = c("Left" = 0.0),
      Region = regions
    )
    interactions <- matrix(
      rep(0, 2*(target_levels + 1)), 2, (target_levels + 1),
      dimnames = list(c("Right", "Left"), c("None", names(regions)))
    )
    regions_probs <- setNames(
      rep(1/(target_levels + 1), (target_levels + 1)),
      c("None", names(regions))
    )
    cj <- ConjointSim$new(
      levels = list(
        Party = c("Right" = 1/2, "Left" = 1/2),
        Region = regions_probs
      ),
      amces = amces,
      interactions = interactions,
      n_tasks = tasks_per_respondent
    )
    # Calculate fixed-N conjoint power (see Schuessler & Freitag)
    exp_size <- cjpowr_amce(
      amce,
      alpha = significance_level,
      power = target_power,
      levels = target_levels
    )[["n"]]/2 # We divide by two because we want N = Respondent x Task NOT N = Resp. x Task x (Profile = 2)

    conjoint_sim_power <- cj$power(
      n_sim = number_of_simulations,
      alpha = significance_level,
      experiment_size = 5 * exp_size
    )
    conjoint_sim_power <- conjoint_sim_power |> 
      mutate(target = .env$amce, fixed_n = ceiling(exp_size))

    return(conjoint_sim_power)
  }
)

power_df <- bind_rows(power)

power_df <- power_df |>
  filter(attribute == "Region") |>
  mutate(
    stat_sig = 0 < conf.low | 0 > conf.high,
    overshoot_ratio = round(amce / target, 2)
  ) |>
  group_by(attribute, level, sim_iter, amce, target) |>
  summarize(
    type2_error = all(!stat_sig),
    early_stop = if (any(stat_sig)) {
      i[min(which(stat_sig))]
    } else {
      first(fixed_n)
    },
    fixed_n = first(fixed_n),
    overshoot_ratio = first(overshoot_ratio),
    .groups = "drop_last"
  ) |>
  ungroup() |>
  # We group by overshoot_ratio to summarize across all target AMCEs
  # If we want separate curves by each target AMCE, do the following:
  # group_by(attribute, level, amce, target)
  group_by(attribute, level, overshoot_ratio) |>
  summarize(
    power = mean(!type2_error),
    power_se = sd(!type2_error)/sqrt(n()),
    power_lb = power - 1.96*power_se,
    power_ub = power + 1.96*power_se,
    pct_sample_save = mean(1 - early_stop/fixed_n),
    pct_sample_save_se = sd(1 - early_stop/fixed_n)/sqrt(n()),
    pct_sample_save_lb = pct_sample_save - 1.96*pct_sample_save_se,
    pct_sample_save_ub = pct_sample_save + 1.96*pct_sample_save_se,
    fixed_n = first(fixed_n),
    n = n(),
    .groups = "drop_last"
  ) |>
  ungroup()

suppressMessages({
  write_fst(power_df, here("data", "figure4.fst"), compress = 100)
})
