library(avlm)
library(dplyr)
library(fixest)
library(future)
library(future.apply)
library(here)
library(progressr)
library(readr)
library(tidyr)

source(here("code", "cj.R"))

options(
  future.globals.maxSize = Inf,
  progressr.enable = TRUE
)

# Specify conjoint parameters
tasks_per_respondent <- 1
significance_level <- 0.05
number_of_simulations <- 1000
experiment_size <- 10000
chunk_size <- 100

# Function that runs one experiment simulation and calculates any occurrences
# of false positives
false_positive <- function(number_of_respondents) {
  amces <- list(
    Party = c("Left" = 0.0),
    Region = c("South" = 0.0)
  )
  interactions <- matrix(
    rep(0, 8), 2, 4,
    dimnames = list(c("Right", "Left"), c("North", "South", "East", "West"))
  )
  cj <- ConjointSim$new(
    levels = list(
      Party = c("Right" = 1/2, "Left" = 1/2),
      Region = c("North" = 1/2, "South" = 1/2)
    ),
    amces = amces,
    interactions = interactions,
    n_tasks = tasks_per_respondent
  )
  # Simulate the conjoint
  conjoint_data <- tibble()
  conjoint_estimates <- tibble()
  for (index in seq(chunk_size, 10000, by = chunk_size)) {
    chunk_data <- cj$sample(n_respondents = chunk_size)
    conjoint_data <- bind_rows(conjoint_data, chunk_data)
    # Fit model
    cj_model <- feols(chosen ~ Party + Region, data = conjoint_data, cluster = ~ resp_id)
    # Tidy the results; AV and fixed-N
    cj_estimates_av <- av_tidy(
      cj_model,
      g = optimal_g(nrow(conjoint_data), length(coef(cj_model)), significance_level),
      alpha = significance_level
    ) |>
      filter(term != "(Intercept)") |>
      mutate(i = index, which = "av")
    cj_estimates_fixed <- tidy(cj_model, conf.int = TRUE) |> 
      filter(term != "(Intercept)") |>
      mutate(i = index, which = "fixed")
    cj_estimates <- bind_rows(cj_estimates_av, cj_estimates_fixed) |>
      separate("term", into = c("attribute", "level"), sep = "(?<=[a-z])(?=[A-Z])") |>
      filter(attribute == "Region") |>
      mutate(stat_sig = p.value < significance_level)
    # Merge all results thus far
    conjoint_estimates <- bind_rows(conjoint_estimates, cj_estimates)
  }
  # Estimate if any false positives have been detected
  conjoint_estimates |> 
    group_by(which) |>
    mutate(any_false_positive = cumany(stat_sig)) |>
    select(attribute, level, estimate, p.value, i, which, any_false_positive) |>
    ungroup()
}

## Calculate the cumulative Type 1 error curves -------------------------------

set.seed(476816)
plan(multicore)
with_progress({
  pb <- progressor(along = 1:number_of_simulations)
  false_positive_sims <- future_lapply(
    1:number_of_simulations,
    function(iter) {
      sim <- false_positive(experiment_size) |> mutate(sim_iter = iter)
      pb()
      return(sim)
    },
    future.seed = TRUE
  )
})
plan(sequential)

false_positives <- bind_rows(false_positive_sims) |>
  summarize(
    p_false_positive = mean(any_false_positive),
    p_fp_se = sd(any_false_positive)/sqrt(number_of_simulations),
    p_false_positive_upper = p_false_positive + 1.96*p_fp_se,
    p_false_positive_lower = p_false_positive - 1.96*p_fp_se,
    .by = c(which, i)
  ) |>
  mutate(which = case_when(which == "av" ~ "Anytime-valid", TRUE ~ "Conventional")) |>
  rename(Method = which)

write_csv(false_positives, here("data", "figure2"))
