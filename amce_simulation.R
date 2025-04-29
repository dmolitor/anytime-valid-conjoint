library(dplyr)
library(fixest)
library(future)
library(future.apply)
library(ggplot2)
library(here)
library(marginaleffects)
library(progressr)
library(readr)
library(scales)

source(here("src/anytime_valid_conjoint/simulation.R"))
source(here("src/anytime_valid_conjoint/utils.R"))

options(future.globals.maxSize = Inf)

# Specify the attribute-level AMCEs for attribute 1 & 2
attr_party <- c("Left" = 0.1)
attr_region <- c("South" = -0.01, "East" = -0.075, "West" = 0.05)

## Simulate conjoint experiment -- estimating AMCEs

conjoint_sim <- simulate(
  levels = list(
    party = c("Right", "Left"),
    region = c("North", "South", "East", "West")
  ),
  amce_fn = !!expr(
    !!attr_region[["South"]] * (region == "South") +
    !!attr_region[["East"]] * (region == "East") +
    !!attr_region[["West"]] * (region == "West") +
    !!attr_party[["Left"]] * (party == "Left")
  ),
  formula = outcome ~ region + party,
  n = 5000,
  skip = 30,
  parallel = TRUE
)

truth <- bind_rows(
  mutate(
    as_tibble(data.frame("estimate_truth" = attr_party), rownames = "contrast"),
    "term" = "party",
    .before = 0
  ),
  mutate(
    as_tibble(data.frame("estimate_truth" = attr_region), rownames = "contrast"),
    "term" = "region",
    .before = 0
  )
)

amce_sim_plot <- ggplot(
    conjoint_sim |> 
      left_join(truth, by = c("term", "contrast")) |>
      filter(!is.na(estimate_truth)) |>
      mutate(
        term = case_when(term == "party" ~ "Party", term == "region" ~ "Region", TRUE ~ term),
        contrast_x_term = paste0(term, ": ", contrast)
      ) |>
      group_by(term, contrast, estimate_truth) |>
      arrange(iter, .by_group = TRUE) |>
      mutate(
        stat_sig = 0 < cs_lower | 0 > cs_upper,
        early_stop = if (any(stat_sig)) {
          min(iter[stat_sig])
        } else {
          NA
        }
      ),
    aes(x = iter, y = estimate, ymin = cs_lower, ymax = cs_upper)
  ) +
  geom_hline(aes(yintercept = estimate_truth), linetype = "dashed", color = "red") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(aes(xintercept = early_stop), linetype = "dashed", color = "blue") +
  geom_line() +
  geom_ribbon(alpha = 0.3) +
  facet_wrap(
    ~ contrast_x_term,
    nrow = 2
  ) +
  coord_cartesian(ylim = c(-.25, .25)) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90")
  ) +
  labs(x = "N Respondents", y = "AMCE")

ggsave(
  here("figures", "amce_simulation.png"),
  plot = amce_sim_plot,
  width = 6,
  height = 4,
  dpi = 500
)

## Simulate conjoint experiment -- estimating marginal means ------------------

conjoint_sim_mm <- simulate_mm(
  levels = list(
    party = c("Right", "Left"),
    region = c("North", "South", "East", "West")
  ),
  amce_fn = !!expr(
    !!attr_region[["South"]] * (region == "South") +
    !!attr_region[["East"]] * (region == "East") +
    !!attr_region[["West"]] * (region == "West") +
    !!attr_party[["Left"]] * (party == "Left")
  ),
  formula = outcome ~ region + party,
  n = 5000,
  skip = 30,
  parallel = TRUE
)

# Calculate true marginal means
compute_marginal_means <- function(coefs, target) {
  # coefs: named list of numeric vectors (each vector’s names are its levels)
  # target: the name of one element in coefs whose marginal means you want
  stopifnot(target %in% names(coefs))
  # 1) win‐probability for U1–U2 difference
  g <- function(d) {
    res <- d + 0.5 - 0.5 * d^2
    res[d <= -1] <- 0
    res[d >=  1] <- 1
    res
  }
  # 2) competitor utility combos
  comp_grid <- expand.grid(
    lapply(coefs, names),
    stringsAsFactors = FALSE
  )
  comp_mat  <- sapply(names(coefs), function(a) coefs[[a]][ comp_grid[[a]] ])
  comp_vals <- rowSums(comp_mat)
  # 3) focal “other‐attributes” combos
  others     <- setdiff(names(coefs), target)
  if (length(others)>0) {
    focal_grid <- expand.grid(
      lapply(coefs[others], names),
      stringsAsFactors = FALSE
    )
    focal_mat  <- do.call(
      cbind,
      lapply(others, function(a) coefs[[a]][ focal_grid[[a]] ])
    )
  } else {
    focal_mat <- matrix(0, nrow = 1, ncol = 1)
  }
  # 4) for each level of target, compute E[Y|target=lev]
  levels_t   <- names(coefs[[target]])
  mm         <- numeric(length(levels_t))
  for (i in seq_along(levels_t)) {
    lev    <- levels_t[i]
    s_f    <- coefs[[target]][lev] + rowSums(focal_mat)
    p_f    <- vapply(s_f, function(sf) mean(g(sf - comp_vals)), numeric(1))
    mm[i]  <- mean(p_f)
  }
  tibble(contrast = levels_t, estimate_truth = mm)
}

truth_mm <- bind_rows(
  lapply(
    c("region", "party"),
    function(term) {
      compute_marginal_means(
        list(
          region = c("North" = 0, attr_region),
          party = c("Right" = 0, attr_party)
        ),
        term
      ) |>
      mutate(term = term)
    }
  )
)

# Plot it!
mm_sim_plot <- ggplot(
  conjoint_sim_mm |> 
    left_join(truth_mm, by = c("term", "contrast")) |>
    mutate(term = case_when(term == "party" ~ "Party", term == "region" ~ "Region", TRUE ~ term)),
  aes(x = iter, y = estimate, ymin = cs_lower, ymax = cs_upper)
) +
  geom_hline(aes(yintercept = estimate_truth), linetype = "dashed", color = "red") +
  geom_line() +
  geom_ribbon(alpha = 0.3) +
  facet_wrap(~ term + contrast, nrow = 2) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90")
  ) +
  labs(x = "N", y = "Marginal Means")

ggsave(
  here("figures", "mm_simulation.png"),
  plot = mm_sim_plot,
  width = 6,
  height = 5
)

# Coverage error simulation; AMCEs and MMs ------------------------------------

n_sim <- 1000
plan(multicore)
coverage_sim <- bind_rows(
  with_progress({
    pb <- progressor(along = 1:n_sim)
    future_lapply(
      1:n_sim,
      function(i) {
        sim_results <- simulate(
          levels = list(
            party = c("Right", "Left"),
            region = c("North", "South", "East", "West")
          ),
          amce_fn = !!expr(
            !!attr_region[["South"]] * (region == "South") +
            !!attr_region[["East"]] * (region == "East") +
            !!attr_region[["West"]] * (region == "West") +
            !!attr_party[["Left"]] * (party == "Left")
          ),
          formula = outcome ~ region + party,
          n = 5000,
          skip = 30,
          parallel = FALSE
        )
        sim_results <- sim_results |> mutate(sim_iter = i)
        pb()
        return(sim_results)
      },
      future.seed = TRUE,
      future.globals = list(attr_region = attr_region)
    )
  })
)

coverage_sim_mm <- bind_rows(
  with_progress({
    pb <- progressor(along = 1:n_sim)
    future_lapply(
      1:n_sim,
      function(i) {
        sim_results <- simulate_mm(
          levels = list(
            party = c("Right", "Left"),
            region = c("North", "South", "East", "West")
          ),
          amce_fn = !!expr(
            !!attr_region[["South"]] * (region == "South") +
            !!attr_region[["East"]] * (region == "East") +
            !!attr_region[["West"]] * (region == "West") +
            !!attr_party[["Left"]] * (party == "Left")
          ),
          formula = outcome ~ region + party,
          n = 5000,
          skip = 30,
          parallel = FALSE
        )
        sim_results <- sim_results |> mutate(sim_iter = i)
        pb()
        return(sim_results)
      }
    )
  })
)
plan(sequential)

coverage_sim_amce_df <- left_join(coverage_sim, truth, by = c("term", "contrast")) |>
  filter(!is.na(estimate_truth)) |>
  mutate(which = "AMCE")
coverage_sim_mm_df <- left_join(coverage_sim_mm, truth_mm, by = c("term", "contrast")) |>
  filter(!is.na(estimate_truth)) |>
  mutate(which = "Marginal Mean")
coverage_sim_df <- bind_rows(coverage_sim_amce_df, coverage_sim_mm_df)
write_csv(coverage_sim_df, here("data", "coverage_sim.csv"))

error_rates <- coverage_sim_df |>
  mutate(covered = estimate_truth >= cs_lower & estimate_truth <= cs_upper) |>
  summarize(
    error = !all(covered),
    .by = c(sim_iter, term, contrast, reference, which)
  ) |>
  summarize(
    error_rate = mean(error),
    error_rate_se = sd(error)/sqrt(n_sim),
    .by = c(term, contrast, reference, which)
  )
print(error_rates)

alpha <- 0.05
plot_coverage <- function(error_rates, var = "both") {
  if (var != "both") {
    error_rates <- error_rates |> filter(which == var)
  }
  coverage_error_plot <- error_rates |>
    mutate(
      term = case_when(
        term == "party" ~ "Party",
        term == "region" ~ "Region",
        TRUE ~ term
      ),
      label = paste0(term, " - ", contrast),
      upper = error_rate + qnorm(1-alpha/2)*error_rate_se,
      lower = error_rate - qnorm(1-alpha/2)*error_rate_se
    ) |>
    ggplot(aes(
      x = label,
      y = error_rate,
      ymin = lower,
      ymax = upper,
      color = if (var == "both") which else NULL
    )) +
    geom_point(position = position_dodge(width = 0.25)) +
    geom_linerange(position = position_dodge(width = 0.25)) +
    geom_hline(yintercept = alpha, linetype = "dashed", color = "red") +
    theme_minimal() +
    coord_flip() +
    scale_y_continuous(
      labels = percent,
      limits = c(0, 0.075),
      breaks = seq(0, 0.075, length.out = 4)
    ) +
    labs(x = "", y = "Coverage error rate", color = "")
  return(coverage_error_plot)
}
coverage_error_plot_amce <- plot_coverage(error_rates, "AMCE")

ggsave(
  here("figures", "coverage_error_rates_amce.png"),
  plot = coverage_error_plot_amce,
  width = 4,
  height = 2
)
