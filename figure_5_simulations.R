library(dplyr)
library(future)
library(future.apply)
library(here)
library(progressr)
library(readr)

source(here("cj.R"))

options(future.globals.maxSize = Inf)

##  Setup conjoint object

# Specify the attribute-level AMCEs for attribute 1 & 2
amces <- list(
  Party = c("Left" = 0.1),
  Region = c("South" = -0.01, "East" = -0.075, "West" = 0.05)
)
interactions <- matrix(
  rep(0, 8), 2, 4,
  dimnames = list(c("Right", "Left"), c("North", "South", "East", "West"))
)

# Specify conjoint parameters
tasks_per_respondent = 2
number_of_respondents = 2500
significance_level = 0.05

# Define the conjoint setup
cj <- ConjointSim$new(
  levels = list(
    Party = c("Right" = 1/2, "Left" = 1/2),
    Region = c("North" = 1/4, "South" = 1/4, "East" = 1/4, "West" = 1/4)
  ),
  amces = amces,
  interactions = interactions,
  n_tasks = tasks_per_respondent
)

## Figure 5 -------------------------------------------------------------------

# How many simulations to run
n_sim <- 100#0

# Run coverage error rate simulations in parallel
plan(multicore)
coverage_sim <- bind_rows(
  with_progress({
    pb <- progressor(along = 1:n_sim)
    future_lapply(
      1:n_sim,
      function(x) {
        coverage_cj <- ConjointSim$new(
          levels = list(
            Party = c("Right" = 1/2, "Left" = 1/2),
            Region = c("North" = 1/4, "South" = 1/4, "East" = 1/4, "West" = 1/4)
          ),
          amces = amces,
          interactions = interactions,
          n_tasks = tasks_per_respondent
        )
        # Simulate the conjoint
        coverage_cj$simulate_conjoint(
          alpha = significance_level,
          experiment_size = number_of_respondents,
          chunk_size = 50
        )
        sim_results <- coverage_cj$estimates |> mutate(sim_iter = x)
        pb()
        return(sim_results)
      },
      future.seed = TRUE
    )
  })
)
plan(sequential)

# Save results locally
write_csv(coverage_sim, here("data", "figure5.csv"))
