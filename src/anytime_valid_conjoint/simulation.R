library(DeclareDesign)
library(fixest)
library(future)
library(future.apply)
library(marginaleffects)
library(progressr)
library(rdss)
library(tidyr)

simulate <- function(
  levels,
  amce_fn,
  formula,
  n = 1000,
  n_warmup = 100,
  skip = 100,
  tasks = 1,
  parallel = TRUE
) {
  amce_fn <- enexpr(amce_fn)
  conjoint_design <- expr(
    declare_model(
      subject = add_level(N = n),                # Number of subjects
      pair = add_level(N = tasks),               # tasks per subject
      candidate = add_level(N = 2, U = runif(N)) # 2 candidates per pair; candidate-level error U ~ Uniform(0,1)
    ) + declare_assignment(
      handler = conjoint_assignment,
      levels_list = levels
    ) + declare_step(
      E = !!amce_fn + U,
      handler = fabricate
    ) +
    declare_measurement(
      handler = function(data) {
        data |>
          group_by(pair) |>
          mutate(outcome = if_else(E == max(E), 1, 0)) |>
          ungroup()
      }
    )
  )
  conjoint_design <- eval(conjoint_design)
  # "Gather" survey responses before estimating
  sim_data <- draw_data(conjoint_design)
  # Initialize tibble to collect our AMCE estimates
  if (parallel) plan(multicore)
  sim_estimates <- future_lapply(
    unique(c(seq(n_warmup, n, by = skip), n)),
    function(i) {
      sim_model <- feglm(
        formula,
        data = filter(sim_data, as.numeric(subject) <= i),
        family = "binomial",
        cluster = ~ subject
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
  if (parallel) plan(sequential)
  sim_estimates <- bind_rows(sim_estimates)
  return(sim_estimates)
}

simulate_mm <- function(
  levels,
  amce_fn,
  formula,
  n = 1000,
  n_warmup = 100,
  skip = 100,
  tasks = 1,
  parallel = TRUE
) {
  amce_fn <- enexpr(amce_fn)
  conjoint_design <- expr(
    declare_model(
      subject = add_level(N = n),                # Number of subjects
      pair = add_level(N = tasks),               # tasks per subject
      candidate = add_level(N = 2, U = runif(N)) # 2 candidates per pair; candidate-level error U ~ Uniform(0,1)
    ) + declare_assignment(
      handler = conjoint_assignment,
      levels_list = levels
    ) + declare_step(
      E = !!amce_fn + U,
      handler = fabricate
    ) +
    declare_measurement(
      handler = function(data) {
        data |>
          group_by(pair) |>
          mutate(outcome = if_else(E == max(E), 1, 0)) |>
          ungroup()
      }
    )
  )
  conjoint_design <- eval(conjoint_design)
  # "Gather" survey responses before estimating
  sim_data <- draw_data(conjoint_design)
  # Initialize tibble to collect our AMCE estimates
  if (parallel) plan(multicore)
  sim_estimates <- future_lapply(
    unique(c(seq(n_warmup, n, by = skip), n)),
    function(i) {
      sim_model <- feglm(
        formula,
        data = filter(sim_data, as.numeric(subject) <= i),
        family = "binomial",
        cluster = ~ subject
      )
      # Calculate marginal effects
      mm_sim <- marginal_means(sim_model, names(levels))
      # Calculate sequential p-values and CSs
      mm_sim_seq <- sequential_f_cs(
        delta = mm_sim$estimate,
        se = mm_sim$std.error,
        n = i,
        n_params = length(sim_model$coefficients),
        Z = NULL,
        term = mm_sim$term
      ) |>
        mutate(iter = i, contrast = mm_sim$level)
      return(mm_sim_seq)
    },
    future.seed = TRUE
  )
  if (parallel) plan(sequential)
  sim_estimates <- bind_rows(sim_estimates)
  return(sim_estimates)
}

simulate_fixed <- function(
  levels,
  amce_fn,
  formula,
  n = 1000,
  tasks = 1
) {
  amce_fn <- enexpr(amce_fn)
  conjoint_design <- expr(
    declare_model(
      subject = add_level(N = n),                # Number of subjects
      pair = add_level(N = tasks),               # tasks per subject
      candidate = add_level(N = 2, U = runif(N)) # 2 candidates per pair; candidate-level error U ~ Uniform(0,1)
    ) + declare_assignment(
      handler = conjoint_assignment,
      levels_list = levels
    ) + declare_step(
      E = !!amce_fn + U,
      handler = fabricate
    ) +
    declare_measurement(
      handler = function(data) {
        data |>
          group_by(pair) |>
          mutate(outcome = if_else(E == max(E), 1, 0)) |>
          ungroup()
      }
    )
  )
  conjoint_design <- eval(conjoint_design)
  # "Gather" survey responses before estimating
  sim_data <- draw_data(conjoint_design)
  sim_model <- feglm(
    formula,
    data = sim_data,
    family = "binomial",
    cluster = ~ subject
  )
  # Calculate marginal effects
  marginal_eff_sim <- avg_slopes(sim_model)
  sim_estimates <- tidy(marginal_eff_sim) |>
    add_reference() |>
    rename(
      "std_error" = "std.error",
      "p_value" = "p.value",
      "s_value" = "s.value",
      "cs_lower" = "conf.low",
      "cs_upper" = "conf.high"
    )
  return(sim_estimates)
}

power <- function(
  levels,
  amce_fn,
  formula,
  n = 1000,
  n_warmup = 100,
  skip = 100,
  tasks = 1,
  n_sim = 100,
  parallel = TRUE,
  verbose = TRUE
) {
  power_calc <- function() {
    pb <- progressor(along = 1:n_sim)
    if (parallel) plan(multicore)
    sims <- future_lapply(
      1:n_sim,
      function(sim_iter) {
        sim <- simulate(
          levels = levels,
          amce_fn = !!enexpr(amce_fn),
          formula = formula,
          n = n,
          n_warmup = n_warmup,
          skip = skip,
          tasks = tasks,
          parallel = FALSE
        )
        sim <- mutate(sim, sim_iter = sim_iter)
        pb()
        return(sim)
      },
      future.seed = TRUE,
      future.globals = list(
        levels   = levels,
        amce_fn  = enexpr(amce_fn),
        formula  = formula,
        simulate = simulate,
        progressor = progressor
      )
    )
    sims <- bind_rows(sims)
    if (parallel) plan(sequential)
    return(sims)
  }
  if (verbose) {
    with_progress({
      sim_results <- power_calc()
    })
  } else {
    sim_results <- power_calc()
  }
  return(sim_results)
}

power_fixed <- function(
  levels,
  amce_fn,
  formula,
  n = 1000,
  tasks = 1,
  n_sim = 100,
  parallel = TRUE,
  verbose = TRUE
) {
  power_calc <- function() {
    pb <- progressor(along = 1:n_sim)
    if (parallel) plan(multicore)
    sims <- future_lapply(
      1:n_sim,
      function(sim_iter) {
        sim <- simulate_fixed(
          levels = levels,
          amce_fn = !!enexpr(amce_fn),
          formula = formula,
          n = n,
          tasks = tasks
        )
        sim <- mutate(sim, sim_iter = sim_iter)
        pb()
        return(sim)
      },
      future.seed = TRUE,
      future.globals = list(
        levels   = levels,
        amce_fn  = enexpr(amce_fn),
        formula  = formula,
        simulate = simulate,
        progressor = progressor
      )
    )
    sims <- bind_rows(sims)
    if (parallel) plan(sequential)
    return(sims)
  }
  if (verbose) {
    with_progress({
      sim_results <- power_calc()
    })
  } else {
    sim_results <- power_calc()
  }
  return(sim_results)
}