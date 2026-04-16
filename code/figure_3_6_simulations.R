suppressPackageStartupMessages({
  library(dplyr)
  library(here)
  library(fst)
})

source(here("code", "cj.R"))

options(
  future.globals.maxSize = Inf,
  progressr.enable = TRUE
)

# Specify conjoint parameters
tasks_per_respondent <- 1
significance_level <- 0.05
number_of_simulations <- 50#0
sample_size_grid <- c(6000)#c(3000, 6000, 11000, 18000)
amce_grid <- seq(0.02, 0.02, by = 0.01)#seq(0.02, 0.13, by = 0.01)
attribute_levels_grid <- c(6)#c(4, 6, 9)

## Run anytime-valid efficiency simulations -----------------------------------

set.seed(42074)
sim_efficiency <- lapply(
  attribute_levels_grid,
  function(n_levels) {
    lapply(
      sample_size_grid,
      function(sim_n) {
        lapply(
          amce_grid,
          function(amce) {
            amce_grid <- seq(amce, amce + 0.01, length.out = n_levels + 1)[1:n_levels]
            # Setup the conjoint object
            regions <- setNames(amce_grid, paste("Region", 1:n_levels))
            amces <- list(
              Party = c("Left" = 0.0),
              Region = regions
            )
            interactions <- matrix(
              rep(0, 2*(n_levels + 1)), 2, (n_levels + 1),
              dimnames = list(c("Right", "Left"), c("None", names(regions)))
            )
            regions_probs <- setNames(rep(1/(n_levels + 1), (n_levels + 1)), c("None", names(regions)))
            cj <- ConjointSim$new(
              levels = list(
                Party = c("Right" = 1/2, "Left" = 1/2),
                Region = regions_probs
              ),
              amces = amces,
              interactions = interactions,
              n_tasks = tasks_per_respondent
            )
            # Simulate the conjoint power
            conjoint_sim_power <- cj$power(
              n_sim = number_of_simulations,
              alpha = significance_level,
              chunk_size = 50,
              experiment_size = sim_n
            )
            conjoint_sim_power <- mutate(
              conjoint_sim_power,
              N = sim_n,
              n_lev = n_levels
            )
            return(conjoint_sim_power)
          }
        )
      }
    )
  }
)
sim_efficiency_df <- bind_rows(lapply(sim_efficiency, bind_rows))
suppressMessages({
  write_fst(sim_efficiency_df, here("data", "figure_3_6_av.fst"))
})

## Run fixed-sample efficiency simulations ------------------------------------

sim_efficiency_fixed <- lapply(
  attribute_levels_grid,
  function(n_levels) {
    lapply(
      sample_size_grid,
      function(sim_n) {
        lapply(
          amce_grid,
          function(amce) {
            amce_grid <- seq(amce, amce + 0.01, length.out = n_levels + 1)[1:n_levels]
            # Setup the conjoint object
            regions <- setNames(amce_grid, paste("Region", 1:n_levels))
            amces <- list(
              Party = c("Left" = 0.0),
              Region = regions
            )
            interactions <- matrix(
              rep(0, 2*(n_levels + 1)), 2, (n_levels + 1),
              dimnames = list(c("Right", "Left"), c("None", names(regions)))
            )
            regions_probs <- setNames(rep(1/(n_levels + 1), (n_levels + 1)), c("None", names(regions)))
            cj <- ConjointSim$new(
              levels = list(
                Party = c("Right" = 1/2, "Left" = 1/2),
                Region = regions_probs
              ),
              amces = amces,
              interactions = interactions,
              n_tasks = tasks_per_respondent
            )
            # Simulate the conjoint power
            conjoint_sim_power <- cj$power_fixed(
              n_sim = number_of_simulations,
              alpha = significance_level,
              experiment_size = sim_n
            )
            conjoint_sim_power <- mutate(
              conjoint_sim_power,
              N = sim_n,
              n_lev = n_levels
            )
            return(conjoint_sim_power)
          }
        )
      }
    )
  }
)

sim_efficiency_fixed_df <- bind_rows(lapply(sim_efficiency_fixed, bind_rows))
suppressMessages({
  write_fst(sim_efficiency_fixed_df, here("data", "figure_3_6_fixed.fst"))
})

## Calculate the sample-efficiency of both methods ----------------------------

sample_efficiency_df <- sim_efficiency_df |>
  filter(attribute == "Region", amce <= 0.12001) |> # set to 0.12001 for floating point inclusion
  mutate(stat_sig = 0 < conf.low | 0 > conf.high) |>
  group_by(n_lev, attribute, level, sim_iter, amce, N) |>
  summarize(
    early_stop = if (any(stat_sig)) {
      i[min(which(stat_sig))]
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
  mutate(
    n_lev = factor(paste("Attribute levels:", n_lev)),
    N = factor(paste("N:", N), levels = paste("N:", unique(sim_efficiency_df$N)))
  )

suppressMessages({
  write_fst(sample_efficiency_df, here("data", "figure_3_6_efficiency.fst"))
})
