library(broom)
library(dplyr)
library(fixest)
library(future)
library(future.apply)
library(ggplot2)
library(here)
library(progressr)
library(tibble)

source(here("blog/posts/conjoint_analysis/utils.R"))

# Generate a random draw from an RCT
rct_draw <- function(ate) {
  X1 <- rnorm(1)
  X2 <- rnorm(1)
  X3 <- rnorm(1)
  W <- rbinom(1, 1, 0.5)
  Y <- 0.5 + ate * W - X1 + 1.5*X2 - 0.4*X3 + rnorm(1)
  tibble(Y=Y, X1=X1, X2=X2, X3=X3, W=W)
}

# Simulate an RCT
simulate <- function(ate, n=1e3) {

  # Simulate an RCT with 1000 draws
  rct_data <- bind_rows(lapply(1:10, \(i) rct_draw(ate)))
  rct_estimates <- tibble()

  for (i in 1:n) {
    rct_data <- bind_rows(rct_data, rct_draw(ate))
    model <- feols(Y ~ W + X1 + X2 + X3, data=rct_data)
    coefs <- tidy(model, conf.int = TRUE) |> mutate(which = "Fixed-n")
    coefs_sequential <- sequential_f_cs(
      delta = coefs$estimate,
      se = coefs$std.error,
      n = model$nobs,
      n_params = model$nparams,
      Z = solve(vcov(model)),
      term = coefs$term
    ) |> mutate(which = "Sequential")
    coefs_asymptotic <- sequential_asymptotic_cs(
      delta = coefs[coefs["term"] == "W", "estimate", drop = TRUE],
      n = model$nobs,
      propensity = 0.5,
      sigma_hat = insight::get_sigma(model),
      term = coefs[coefs["term"] == "W", "term", drop = TRUE]
    ) |> mutate(which = "Asymptotic", cs_lower = as.numeric(cs_lower), cs_upper = as.numeric(cs_upper))

    rct_estimates <- bind_rows(
      rct_estimates,
      bind_rows(
        coefs |>
          filter(term == "W") |>
          select(term, estimate, which, cs_lower = conf.low, cs_upper = conf.high),
        coefs_sequential |>
          filter(term == "W") |>
          select(term, estimate, which, cs_lower, cs_upper),
        coefs_asymptotic |> 
          select(term, estimate, which, cs_lower, cs_upper)
      ) |> 
        mutate(index = i)
    )
  }

  return(rct_estimates)
}

# Plot estimates
sim_results <- simulate(ate = 0)
ggplot(sim_results, aes(x = index, y = estimate, ymin = cs_lower, ymax = cs_upper, color = which)) +
  geom_line() +
  geom_linerange(alpha = 0.1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  coord_cartesian(ylim = c(-3, 3)) +
  theme_minimal()

# Type 1 error simulations ----------------------------------------------------

plan(multicore)
with_progress({
  pb <- progressor(along = 1:1000)
  sim_results <- bind_rows(future_lapply(
    1:1000,
    function(i) {
      results <- simulate(ate = 0) |> mutate(sim = i)
      pb()
      return(results)
    },
    future.seed = TRUE
  ))
})
plan(sequential)

# Calculate Type 1 error rate
sim_results |>
  mutate(covered = cs_lower <= 0 & 0 <= cs_upper) |>
  group_by(sim, which) |>
  summarize(error = !all(covered)) |>
  ungroup() |>
  group_by(which) |>
  summarize(error_rate = mean(error))
