library(dplyr)
library(fixest)
library(future)
library(future.apply)
library(ggplot2)
library(here)
library(marginaleffects)
library(progressr)
library(readr)

source(here("src/anytime_valid_conjoint/utils.R"))

# Function to simulate a random survey participant
simulate_profile <- function(amce_attr1, amce_attr2, intercept = 0.5) {
  attr1_levels <- c("Level 1", names(amce_attr1))
  attr2_levels <- c("Level 1", names(amce_attr2))
  # Randomly sample a level for each attribute (uniformly)
  attr1_val <- sample(attr1_levels, 1)
  attr2_val <- sample(attr2_levels, 1)
  # Determine the effect for attribute 1: baseline gets effect 0
  effect1 <- if (attr1_val == "Level 1") 0 else amce_attr1[attr1_val]
  # Determine the effect for attribute 2: baseline gets effect 0
  effect2 <- if (attr2_val == "Level 1") 0 else amce_attr2[attr2_val]
  # Compute the latent probability p
  p <- intercept + effect1 + effect2
  # Ensure that p is within [0, 1]
  stopifnot(p >= 0 && p <= 1)
  # Simulate the binary outcome using p as the success probability.
  outcome <- rbinom(1, size = 1, prob = p)
  return(tibble(
    attr1 = attr1_val,
    attr2 = attr2_val,
    outcome = outcome,
    p = p
  ))
}

# Specify the attribute-level AMCEs for attribute 1 & 2
amce_attr1 <- c("Level 2" = 0.2, "Level 3" = 0.1)
amce_attr2 <- c("Level 2" = -0.1, "Level 3" = 0.15, "Level 4" = 0.3, "Level 5" = -0.2)

simulate <- function(n = 1000, n_warmup = 100, parallel = TRUE) {
  # "Gather" survey responses before estimating
  sim_data <- bind_rows(lapply(1:n, \(i) simulate_profile(amce_attr1, amce_attr2, 0.3)))
  # Initialize tibble to collect our AMCE estimates
  if (parallel) plan(multicore)
  sim_estimates <- future_lapply(
    n_warmup:n,
    function(i) {
      sim_model <- glm(
        outcome ~ attr1 + attr2,
        data = sim_data[1:i, , drop = FALSE],
        family = "binomial"
      )
      # Calculate marginal effects
      marginal_eff_sim <- avg_slopes(sim_model)
      # Calculate sequential p-values and CSs
      marginal_eff_sim_seq <- sequential_f_cs(
        delta = marginal_eff_sim$estimate,
        se = marginal_eff_sim$std.error,
        n = i,
        n_params = length(sim_model$coefficients),
        Z = solve(get_vcov(marginal_eff_sim)),
        term = marginal_eff_sim$term,
        contrast = marginal_eff_sim$contrast
      ) |>
        add_reference(low = "cs_lower", high = "cs_upper") |>
        mutate(iter = i)
      return(marginal_eff_sim_seq)
    },
    future.seed = TRUE
  )
  plan(sequential)
  sim_estimates <- bind_rows(sim_estimates)
  return(sim_estimates)
}

## Simulate conjoint experiment

conjoint_sim <- simulate()

truth <- bind_rows(
  mutate(
    as_tibble(data.frame("estimate_truth" = amce_attr1), rownames = "contrast"),
    "term" = "attr1",
    .before = 0
  ),
  mutate(
    as_tibble(data.frame("estimate_truth" = amce_attr2), rownames = "contrast"),
    "term" = "attr2",
    .before = 0
  )
)

coverage <- left_join(sim_estimates, truth, by = c("term", "contrast")) |>
  filter(!is.na(estimate_truth)) |>
  mutate(covered = estimate_truth >= cs_lower & estimate_truth <= cs_upper) |>
  summarize(error = !all(covered), .by = c(term, contrast, reference))

ggplot(
    conjoint_sim |> 
      left_join(truth, by = c("term", "contrast")) |>
      filter(!is.na(estimate_truth)) |>
      mutate(term = case_when(term == "attr1" ~ "Attribute 1", TRUE ~ "Attribute 2")),
    aes(x = iter, y = estimate, ymin = cs_lower, ymax = cs_upper)
  ) +
  geom_hline(aes(yintercept = estimate_truth), linetype = "dashed", color = "red") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_line() +
  geom_linerange(alpha = 0.1) +
  facet_wrap(~ term + contrast, nrow = 2) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank()
  ) +
  labs(x = "N", y = "AMCE")

ggsave(here("figures", "amce_simulation.png"), width = 6, height = 5, dpi = 300)

## Coverage simulation

coverage_sim <- bind_rows(
  with_progress({
    pb <- progressor(along = 1:100)
    lapply(
      1:100,
      function(i) {
        sim_results <- simulate() |> mutate(sim_iter = i)
        pb()
        return(sim_results)
      }
    )
  })
)

coverage_sim_df <- left_join(coverage_sim, truth, by = c("term", "contrast")) |>
  filter(!is.na(estimate_truth))
write_csv(coverage_sim_df, here("tables", "coverage_sim.csv"))

error_rates <- coverage_sim_df |>
  mutate(covered = estimate_truth >= cs_lower & estimate_truth <= cs_upper) |>
  summarize(error = !all(covered), .by = c(sim_iter, term, contrast, reference)) |>
  summarize(error_rate = mean(error), .by = c(term, contrast, reference))
print(error_rates)
