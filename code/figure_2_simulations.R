suppressPackageStartupMessages({
  library(dplyr)
  library(fixest)
  library(future)
  library(future.apply)
  library(here)
  library(readr)
  library(tidyr)
})

source(here("code", "cj.R"))

options(
  future.globals.maxSize = Inf,
  progressr.enable = TRUE
)
fixest::setFixest_nthreads(1)

# Specify conjoint parameters
tasks_per_respondent <- 1
significance_level <- 0.05
number_of_simulations <- 1000
experiment_size <- 10000
chunk_size <- 100

null_conjoint <- function(region_levels) {
  region_amces <- setNames(rep(0, length(region_levels) - 1), region_levels[-1])
  amces <- list(
    Party = c("Left" = 0.0),
    Region = region_amces
  )
  interactions <- matrix(
    rep(0, 2 * length(region_levels)), 2, length(region_levels),
    dimnames = list(c("Right", "Left"), region_levels)
  )
  ConjointSim$new(
    levels = list(
      Party = c("Right" = 1/2, "Left" = 1/2),
      Region = setNames(rep(1 / length(region_levels), length(region_levels)), region_levels)
    ),
    amces = amces,
    interactions = interactions,
    n_tasks = tasks_per_respondent,
    dgp = "logit",
    respondent_types = c("0" = 1)
  )
}

# Function that runs one experiment simulation and calculates any occurrences
# of false positives for one scalar region coefficient.
false_positive_scalar <- function() {
  cj <- null_conjoint(c("North", "South"))
  g_scalar <- optimal_g(experiment_size, significance_level)

  # Simulate the conjoint
  conjoint_data <- tibble()
  conjoint_estimates <- tibble()
  for (index in seq(chunk_size, experiment_size, by = chunk_size)) {
    chunk_data <- cj$sample(n_respondents = chunk_size)
    conjoint_data <- bind_rows(conjoint_data, chunk_data)
    # Fit model
    cj_model <- feols(chosen ~ Party + Region, data = conjoint_data, cluster = ~ resp_id)
    # Tidy the results; AV and fixed-N
    cj_estimates_av <- av_tidy(
      cj_model,
      alpha = significance_level,
      cluster = conjoint_data$resp_id,
      g = g_scalar
    ) |>
      filter(term != "(Intercept)") |>
      mutate(i = index, which = "av")
    cj_estimates_fixed <- tidy(cj_model, conf.int = TRUE) |> 
      filter(term != "(Intercept)") |>
      mutate(i = index, which = "fixed")
    cj_estimates <- bind_rows(cj_estimates_av, cj_estimates_fixed) |>
      separate("term", into = c("attribute", "level"), sep = "(?<=[a-z])(?=[A-Z])") |>
      filter(attribute == "Region") |>
      mutate(
        stat_sig = p.value < significance_level,
        Test = "Single region coefficient"
      )
    # Merge all results thus far
    conjoint_estimates <- bind_rows(conjoint_estimates, cj_estimates)
  }
  # Estimate if any false positives have been detected
  conjoint_estimates |> 
    group_by(which) |>
    mutate(any_false_positive = cumany(stat_sig)) |>
    select(Test, attribute, level, estimate, p.value, i, which, any_false_positive) |>
    ungroup()
}

# Function that runs one experiment simulation and calculates any occurrences
# of false positives for the joint region null. The conventional fixed-sample
# p-value compares the cluster-robust Wald statistic divided by d to F(d, G-d);
# the anytime-valid p-value uses the d-dimensional cluster-robust e-process.
false_positive_joint <- function() {
  region_levels <- c("North", "South", "East", "West")
  cj <- null_conjoint(region_levels)
  g_joint <- optimal_g_multivariate(
    experiment_size,
    length(region_levels) - 1,
    significance_level
  )

  conjoint_data <- tibble()
  conjoint_estimates <- tibble()
  for (index in seq(chunk_size, experiment_size, by = chunk_size)) {
    chunk_data <- cj$sample(n_respondents = chunk_size)
    conjoint_data <- bind_rows(conjoint_data, chunk_data)
    cj_model <- feols(chosen ~ Party + Region, data = conjoint_data, cluster = ~ resp_id)

    region_terms <- grep("^Region", names(coef(cj_model)), value = TRUE)
    joint_test <- wald_tidy(
      cj_model,
      terms = region_terms,
      alpha = significance_level,
      cluster = conjoint_data$resp_id,
      g = g_joint
    )

    cj_estimates <- bind_rows(
      joint_test |>
        transmute(
          Test = "Joint region coefficients",
          attribute = "Region",
          level = "South/East/West",
          estimate = wald,
          p.value = p.value,
          i = index,
          which = "av",
          stat_sig = p.value < significance_level
        ),
      joint_test |>
        transmute(
          Test = "Joint region coefficients",
          attribute = "Region",
          level = "South/East/West",
          estimate = wald,
          p.value = p.value.fixed,
          i = index,
          which = "fixed",
          stat_sig = p.value < significance_level
        )
    )

    conjoint_estimates <- bind_rows(conjoint_estimates, cj_estimates)
  }

  conjoint_estimates |>
    group_by(which) |>
    mutate(any_false_positive = cumany(stat_sig)) |>
    select(Test, attribute, level, estimate, p.value, i, which, any_false_positive) |>
    ungroup()
}

## Calculate the cumulative Type 1 error curves -------------------------------

set.seed(476816)
plan(multicore)
false_positive_sims <- future_lapply(
  1:number_of_simulations,
  function(iter) {
    bind_rows(
      false_positive_scalar(),
      false_positive_joint()
    ) |>
      mutate(sim_iter = iter)
  },
  future.seed = TRUE
)
plan(sequential)

false_positives <- bind_rows(false_positive_sims) |>
  summarize(
    p_false_positive = mean(any_false_positive),
    p_fp_se = sd(any_false_positive)/sqrt(number_of_simulations),
    p_false_positive_upper = p_false_positive + 1.96*p_fp_se,
    p_false_positive_lower = p_false_positive - 1.96*p_fp_se,
    .by = c(Test, which, i)
  ) |>
  mutate(which = case_when(which == "av" ~ "Anytime-valid", TRUE ~ "Conventional")) |>
  rename(Method = which)

write_csv(false_positives, here("data", "figure2.csv"))
