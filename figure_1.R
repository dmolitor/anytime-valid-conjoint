library(here)
library(ggplot2)
library(readr)

source(here("cj.R"))

options(future.globals.maxSize = Inf)

## Figure 1 -------------------------------------------------------------------

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
# Simulate the conjoint
cj$simulate_conjoint(
  alpha = significance_level,
  experiment_size = number_of_respondents,
  chunk_size = 100
)

# Plot Figure 1
p <- cj$plot_estimates(TRUE, show_when_stat_sig = TRUE) +
  labs(x = "Sample size", y = "AMCE")
# Save the results
ggsave(
  here("figures", "figure1.png"),
  plot = p,
  width = 6,
  height = 4,
  dpi = 500
)