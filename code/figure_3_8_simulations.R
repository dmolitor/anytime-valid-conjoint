zz <- file(nullfile(), open = "wt")

sink(zz)
sink(zz, type = "message")

on.exit({
  while (sink.number(type = "message") > 0) sink(type = "message")
  while (sink.number() > 0) sink()
  close(zz)
}, add = TRUE)

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
fixest::setFixest_nthreads(1)

# Specify conjoint parameters
tasks_per_respondent <- 2
significance_level <- 0.05
number_of_simulations <- 500
sample_size_grid <- c(3000, 6000, 11000, 18000)
amce_grid <- seq(0.02, 0.13, by = 0.01)
attribute_levels_grid <- c(4, 6, 9)
n_parallel_workers <- 12 # Change this to fewer workers if running out of memory

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
            # Scale a proportionally spaced utility-coefficient vector so that
            # its first realized AMCE matches the nominal grid value (see cj.R).
            regions <- solve_logit_region_coefs(amce, n_levels)
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
              n_tasks = tasks_per_respondent,
              dgp = "logit"
            )
            # Simulate the conjoint power
            conjoint_sim_power <- cj$power(
              n_sim = number_of_simulations,
              alpha = significance_level,
              chunk_size = 100,
              experiment_size = sim_n,
              n_workers = n_parallel_workers
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
  write_fst(sim_efficiency_df, here("data", "figure_3_8_av.fst"))
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
            # Scale a proportionally spaced utility-coefficient vector so that
            # its first realized AMCE matches the nominal grid value (see cj.R).
            regions <- solve_logit_region_coefs(amce, n_levels)
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
              n_tasks = tasks_per_respondent,
              dgp = "logit"
            )
            # Simulate the conjoint power
            conjoint_sim_power <- cj$power_fixed(
              n_sim = number_of_simulations,
              alpha = significance_level,
              experiment_size = sim_n,
              n_workers = n_parallel_workers
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
  write_fst(sim_efficiency_fixed_df, here("data", "figure_3_8_fixed.fst"))
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
  write_fst(sample_efficiency_df, here("data", "figure_3_8.fst"))
})
