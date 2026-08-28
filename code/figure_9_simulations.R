suppressPackageStartupMessages({
  library(dplyr)
  library(future)
  library(future.apply)
  library(here)
  library(readr)
})

source(here("code", "cj.R"))

options(future.globals.maxSize = Inf)
fixest::setFixest_nthreads(1)

##  Setup conjoint object

# Specify utility coefficients for the nonlinear forced-choice DGP.
# The exact AMCEs are computed by finite summation in `compute_true_amces()`.
utility_effects <- list(
  Party = c("Left" = 0.35),
  Region = c("South" = -0.06, "East" = -0.27, "West" = 0.16)
)
utility_interactions <- matrix(
  c(0, 0, 0, 0,
    0, 0.04, -0.05, 0.03),
  2, 4,
  byrow = TRUE,
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
  amces = utility_effects,
  interactions = utility_interactions,
  n_tasks = tasks_per_respondent,
  dgp = "logit"
)

## Figure 5 -------------------------------------------------------------------

# How many simulations to run
n_sim <- 1000

# Run coverage error rate simulations in parallel
set.seed(563329)
plan(multicore)
coverage_sim <- bind_rows(
  future_lapply(
    1:n_sim,
    function(x) {
      coverage_cj <- ConjointSim$new(
        levels = list(
          Party = c("Right" = 1/2, "Left" = 1/2),
          Region = c("North" = 1/4, "South" = 1/4, "East" = 1/4, "West" = 1/4)
        ),
        amces = utility_effects,
        interactions = utility_interactions,
        n_tasks = tasks_per_respondent,
        dgp = "logit"
      )
      # Simulate the conjoint
      coverage_cj$simulate_conjoint(
        alpha = significance_level,
        experiment_size = number_of_respondents,
        chunk_size = 50
      )
      sim_results <- coverage_cj$estimates |> mutate(sim_iter = x)
      return(sim_results)
    },
    future.seed = TRUE
  )
)
plan(sequential)

# Save results locally
write_csv(coverage_sim, here("data", "figure9.csv"))
