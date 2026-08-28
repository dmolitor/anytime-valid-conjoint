suppressPackageStartupMessages({
  library(broom)
  library(dplyr)
  library(fixest)
  library(ggplot2)
  library(future.apply)
  library(R6)
  library(tidyr)
})

# Anytime-valid e-value/confidence-sequence math (Lindon, Ham, Tingley & Bojinov,
# JASA 2026 / arXiv:2210.08589). Ported locally so the AV formulas can be applied
# directly to a fitted `feols` model's own cluster-robust ("CRV1") point estimate
# and standard error, rather than depending on the `avlm` package.
#
# Per Cameron, Gelbach & Miller (2011), when standard errors are cluster-robust,
# the reference distribution for the AV formulas must use nu = G - 1 (clusters
# minus one) as the denominator degrees of freedom, not n - p (observations
# minus coefficients). Clusters, not rows, are also the unit that indexes the
# anytime-valid sequential process itself, so the accumulating sample size in
# these formulas is G (number of clusters), not the row count.

t_radius <- function(g, G, alpha) {
  d <- 1
  nu <- G - 1
  T <- g / (g + G)
  powered_term <- (T * alpha^2)^(1 / (nu + d))
  numerator <- nu * (1 - powered_term)
  denominator <- max(0, powered_term - T)
  sqrt(numerator / denominator)
}

log_G_t <- function(t2, G, g) {
  nu <- G - 1
  r <- g / (g + G)
  0.5 * log(r) + (0.5 * (nu + 1)) * (log(1 + t2 / nu) - log(1 + r * t2 / nu))
}

p_G_t <- function(log_G_t_values) {
  pmin(1.0, exp(-log_G_t_values))
}

f_radius <- function(g, G, d, alpha) {
  nu <- G - d
  if (nu <= 0) stop("G must be greater than d.")
  T <- g / (g + G)
  powered_term <- (T * alpha^(2 / d))^(d / (nu + d))
  numerator <- (nu / d) * (1 - powered_term)
  denominator <- max(0, powered_term - T)
  numerator / denominator
}

log_G_multivariate_t <- function(Q, G, g, d) {
  nu <- G - d
  if (nu <= 0) stop("G must be greater than d.")
  r <- g / (g + G)
  (d / 2) * log(r) +
    (0.5 * (nu + d)) * (log(1 + Q / nu) - log(1 + r * Q / nu))
}

optimal_g_multivariate <- function(G, d, alpha) {
  if (G <= d) stop("G must be greater than d.")
  if (alpha < 0 || alpha > 1) stop("alpha must be in (0,1).")

  nu <- G - d
  upper_bound <- G * alpha^(2 / nu) / (1 - alpha^(2 / nu))
  lower_bound <- 1

  opt_result <- optimize(
    f_radius,
    interval = c(lower_bound, upper_bound),
    G = G,
    d = d,
    alpha = alpha
  )
  opt_result$minimum
}

wald_tidy <- function(model, terms, alpha, cluster, g = NULL) {
  beta <- coef(model)[terms]
  V <- vcov(model)[terms, terms, drop = FALSE]
  G <- nlevels(as.factor(cluster))
  d <- length(terms)
  if (is.null(g)) g <- optimal_g_multivariate(G, d, alpha)

  Q <- as.numeric(crossprod(beta, solve(V, beta)))
  F_stat <- Q / d
  fixed_p <- stats::pf(F_stat, df1 = d, df2 = G - d, lower.tail = FALSE)
  av_p <- p_G_t(log_G_multivariate_t(Q, G, g, d))

  tibble::tibble(
    term = paste(terms, collapse = ", "),
    statistic = F_stat,
    wald = Q,
    df1 = d,
    df2 = G - d,
    p.value = av_p,
    p.value.fixed = fixed_p
  )
}

# Value of g that minimizes the confidence-sequence radius (t_radius) for a
# given number of clusters G.
optimal_g <- function(G, alpha) {
  if (G <= 1) stop("G must be greater than 1.")
  if (alpha < 0 || alpha > 1) stop("alpha must be in (0,1).")

  nu <- G - 1
  upper_bound <- G * alpha^(2 / nu) / (1 - alpha^(2 / nu))
  lower_bound <- 1

  opt_result <- optimize(
    t_radius,
    interval = c(lower_bound, upper_bound),
    G = G,
    alpha = alpha
  )
  opt_result$minimum
}

# Tidies a `feols` model's cluster-robust estimate/SE into anytime-valid
# sequential p-values and confidence-sequence bounds. G (the number of
# clusters) is the accumulating sample size that indexes anytime-validity here
# and also fixes the reference degrees of freedom to G - 1. If `g` is not
# supplied, uses the value that minimizes the confidence-sequence radius.
av_tidy <- function(model, alpha, cluster, g = NULL) {
  beta <- coef(model)
  se <- sqrt(diag(vcov(model)))
  G <- nlevels(as.factor(cluster))
  if (is.null(g)) g <- optimal_g(G, alpha)

  t <- beta / se
  t2 <- t^2
  p_value <- p_G_t(log_G_t(t2, G, g))
  radius <- se * t_radius(g, G, alpha)

  tibble::tibble(
    term = names(beta),
    estimate = beta,
    std.error = se,
    statistic = t,
    p.value = p_value,
    conf.low = beta - radius,
    conf.high = beta + radius
  )
}

level_values <- function(x) {
  if (is.numeric(x)) names(x) else as.character(x)
}

level_probabilities <- function(x) {
  if (is.numeric(x)) {
    probs <- as.numeric(x)
    names(probs) <- names(x)
    probs / sum(probs)
  } else {
    levs <- as.character(x)
    probs <- rep(1 / length(levs), length(levs))
    names(probs) <- levs
    probs
  }
}

profile_grid <- function(levels) {
  attrs <- names(levels)
  levs <- lapply(levels, level_values)
  probs <- lapply(levels, level_probabilities)

  grid <- expand.grid(levs, stringsAsFactors = FALSE)
  names(grid) <- attrs
  grid <- as_tibble(grid)
  grid$.prob <- Reduce(
    `*`,
    Map(function(a) probs[[a]][grid[[a]]], attrs)
  )
  grid
}

respondent_type_distribution <- function(respondent_types) {
  if (is.null(respondent_types)) {
    respondent_types <- c("-0.45" = 0.25, "0" = 0.50, "0.45" = 0.25)
  }
  if (is.null(names(respondent_types))) {
    probs <- rep(1 / length(respondent_types), length(respondent_types))
    values <- as.numeric(respondent_types)
  } else {
    probs <- as.numeric(respondent_types)
    values <- as.numeric(names(respondent_types))
  }
  probs <- probs / sum(probs)
  list(values = values, probs = probs)
}

profile_score <- function(prof_df, amces, interactions = NULL) {
  attrs <- names(prof_df)
  score <- numeric(nrow(prof_df))

  for (a in attrs) {
    v <- amces[[a]]
    if (!is.null(v) && length(v)) {
      add <- unname(v[prof_df[[a]]])
      add[is.na(add)] <- 0
      score <- score + add
    }
  }

  if (!is.null(interactions) && length(attrs) >= 2) {
    attr1 <- attrs[[1]]
    attr2 <- attrs[[2]]
    rows <- prof_df[[attr1]]
    cols <- prof_df[[attr2]]
    ok <- rows %in% rownames(interactions) & cols %in% colnames(interactions)
    if (any(ok)) {
      score[ok] <- score[ok] + interactions[cbind(rows[ok], cols[ok])]
    }
  }

  score
}

profile_utility <- function(prof_df, levels, amces, interactions = NULL, respondent_taste = 0) {
  attrs <- names(levels)
  prof_attrs <- prof_df[, attrs, drop = FALSE]
  base <- profile_score(prof_attrs, amces, interactions)

  party_nonbaseline <- rep(0, nrow(prof_attrs))
  if ("Party" %in% attrs) {
    party_base <- level_values(levels[["Party"]])[[1]]
    party_nonbaseline <- as.numeric(prof_attrs$Party != party_base)
  }

  region_scaled <- rep(0, nrow(prof_attrs))
  if ("Region" %in% attrs) {
    region_levels <- level_values(levels[["Region"]])
    denom <- max(1, length(region_levels) - 1)
    region_scaled <- (match(prof_attrs$Region, region_levels) - 1) / denom
    region_scaled <- region_scaled - mean(seq(0, 1, length.out = length(region_levels)))
  }

  base +
    0.35 * base^2 -
    0.10 * base^3 +
    0.12 * base * party_nonbaseline +
    respondent_taste * party_nonbaseline +
    0.08 * respondent_taste * region_scaled
}

compute_linear_amces <- function(levels, amces, interactions = NULL) {
  if (is.null(interactions)) {
    out <- lapply(
      names(levels),
      function(x) {
        levs <- level_values(levels[[x]])[-1]
        vals <- unname(amces[[x]][levs])
        vals[is.na(vals)] <- 0
        stats::setNames(vals, levs)
      }
    )
    names(out) <- names(levels)
    return(out)
  }

  out <- lapply(
    names(levels),
    function(x) {
      baseline <- level_values(levels[[x]])[[1]]
      vapply(
        level_values(levels[[x]])[-1],
        function(y) {
          conditioning_var <- names(levels)[!names(levels) == x]
          conditioning_levels <- level_values(levels[[conditioning_var]])
          conditioning_probs <- level_probabilities(levels[[conditioning_var]])

          sum(vapply(
            conditioning_levels,
            function(conditioning_level) {
              if (conditioning_level %in% colnames(interactions)) {
                (
                  (amces[[x]][[y]] + interactions[y, conditioning_level]) -
                    (0 + interactions[baseline, conditioning_level])
                ) * conditioning_probs[[conditioning_level]]
              } else {
                (
                  (amces[[x]][[y]] + interactions[conditioning_level, y]) -
                    (0 + interactions[conditioning_level, baseline])
                ) * conditioning_probs[[conditioning_level]]
              }
            },
            numeric(1)
          ))
        },
        numeric(1)
      )
    }
  )
  names(out) <- names(levels)
  out
}

compute_logit_amces <- function(levels, amces, interactions = NULL, respondent_types = NULL) {
  grid <- profile_grid(levels)
  attrs <- names(levels)
  types <- respondent_type_distribution(respondent_types)

  p_chosen <- numeric(nrow(grid))
  for (k in seq_along(types$values)) {
    own_eta <- profile_utility(grid, levels, amces, interactions, types$values[[k]])
    opp_eta <- own_eta
    p_given_type <- vapply(
      own_eta,
      function(eta) sum(grid$.prob * plogis(eta - opp_eta)),
      numeric(1)
    )
    p_chosen <- p_chosen + types$probs[[k]] * p_given_type
  }

  out <- lapply(
    attrs,
    function(a) {
      levs <- level_values(levels[[a]])
      baseline <- levs[[1]]
      base_prob <- sum(grid$.prob[grid[[a]] == baseline])
      base_mean <- sum(grid$.prob[grid[[a]] == baseline] * p_chosen[grid[[a]] == baseline]) / base_prob
      vapply(
        levs[-1],
        function(lvl) {
          lvl_prob <- sum(grid$.prob[grid[[a]] == lvl])
          lvl_mean <- sum(grid$.prob[grid[[a]] == lvl] * p_chosen[grid[[a]] == lvl]) / lvl_prob
          lvl_mean - base_mean
        },
        numeric(1)
      )
    }
  )
  names(out) <- attrs
  out
}

# This class implements a simple conjoint object. Primarily used for the empirical simulations
ConjointSim <- R6Class(
  "ConjointSim",
  public = list(
    levels = NULL, # list: attr -> (vector of levels OR named prob vector)
    amces = NULL, # utility coefficients for non-baseline levels; baseline=0
    interactions = NULL, # utility interactions: rownames = levels(attr1), colnames = levels(attr2)
    dgp = NULL,
    respondent_types = NULL,
    next_resp_id = NULL,
    next_pair_id = NULL,
    n_tasks = NULL,
    estimates = NULL,

    initialize = function(
      levels,
      amces,
      interactions = NULL,
      n_tasks = 5,
      dgp = "linear",
      respondent_types = c("-0.45" = 0.25, "0" = 0.50, "0.45" = 0.25)
    ) {
      self$levels <- levels
      self$amces <- amces
      self$interactions <- interactions
      self$dgp <- match.arg(dgp, c("logit", "linear"))
      self$respondent_types <- respondent_types
      self$n_tasks <- as.integer(n_tasks)
      self$next_resp_id <- 1L
      self$next_pair_id <- 1L
    },

    plot_estimates = function(uniform_only = FALSE, show_when_stat_sig = TRUE) {
      if (show_when_stat_sig) {
        estimates <- self$estimates |>
          mutate(stat_sig = conf.low > 0 | conf.high < 0) |>
          group_by(attribute, level) |>
          mutate(
            true_from_here_on = rev(cumall(rev(stat_sig))),
            first_stat_sig = if (any(true_from_here_on)) min(i[true_from_here_on]) else NA_integer_
          ) |>
          ungroup()
      } else {
        estimates <- self$estimates
      }
      truth_lines <- estimates |>
        distinct(attribute, level, amce)
      p <- ggplot(
          estimates,
          aes(x = i, y = estimate, ymin = conf.low, ymax = conf.high)
        ) +
        geom_line() +
        geom_ribbon(alpha = 0.2)
      if (show_when_stat_sig) {
        p <- p + geom_vline(
          aes(xintercept = first_stat_sig),
          linetype = "dashed",
          color = "blue"
        )
      }
      p <- p + geom_hline(
        aes(yintercept = amce),
        data = truth_lines,
        color = "red",
        linetype = "dashed"
      ) +
        geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
        facet_wrap(~ paste0(attribute, " - ", level), ncol = 2) +
        theme_minimal() +
        theme(
          panel.grid.minor = element_blank(),
          plot.title = element_text(face = "bold", hjust = 0.5)
        ) +
        coord_cartesian(ylim = c(-0.25, 0.25))
      return(p)
    },

    plot_probabilities = function() {
      ggplot(self$probabilities, aes(x = iter, y = value, color = paste0(name, " - ", value_id))) +
        geom_point() +
        geom_line() +
        facet_wrap(~ name, scales = "free_y", ncol = 1) +
        theme_minimal() +
        labs(x = "Respondent clusters (G)", y = "Assignment probability", color = "")
    },

    probabilities = tibble::tibble(),

    set_levels = function(levels) {
      self$levels <- levels
      invisible(self)
    },

    set_amces = function(amces) {
      self$amces <- amces
      invisible(self)
    },

    set_interactions = function(interactions) {
      self$interactions <- interactions
      invisible(self)
    },

    sample = function(n_respondents) {
      attrs <- names(self$levels)

      # resolve levels + probs (uniform if probs not provided)
      levs <- probs <- setNames(vector("list", length(attrs)), attrs)
      for (a in attrs) {
        x <- self$levels[[a]]
        if (is.numeric(x)) {
          levs[[a]] <- names(x)
          probs[[a]] <- as.numeric(x)
        } else {
          levs[[a]] <- as.character(x)
          probs[[a]] <- rep(1/length(x), length(x))
        }
      }

      # named prob lookups per attribute level
      prob_lookup <- lapply(attrs, function(a) setNames(probs[[a]], levs[[a]]))
      names(prob_lookup) <- attrs

      n_pairs <- n_respondents * self$n_tasks
      n_rows  <- 2 * n_pairs
      resp_ids <- seq.int(self$next_resp_id, length.out = n_respondents)
      pair_ids <- seq.int(self$next_pair_id, length.out = n_pairs)
      self$next_resp_id <- self$next_resp_id + n_respondents
      self$next_pair_id <- self$next_pair_id + n_pairs

      # vectorized profile draws
      draw_attr <- function(a) sample(levs[[a]], n_rows, TRUE, probs[[a]])
      prof_df <- as_tibble(setNames(lapply(attrs, draw_attr), attrs))

      if (self$dgp == "linear") {
        m <- profile_score(prof_df, self$amces, self$interactions)
        alpha <- rep(runif(n_respondents, 0, 0.25), each = 2 * self$n_tasks)
        m <- m + alpha

        if (any(m < -0.1 | m > 0.4)) {
          stop("Score function m(x) has too large of values; shrink amces or interactions.")
        }
        q <- 0.2 + 2 * m

        y <- rbinom(n_rows, 1, q)
        Y <- matrix(y, ncol = 2, byrow = TRUE)
        ties <- (Y[,1] == Y[,2])
        win <- integer(n_pairs)
        win[!ties] <- ifelse(Y[!ties, 1] > Y[!ties, 2], 1, 2)
        win[ties]  <- sample.int(2, sum(ties), TRUE)
      } else {
        types <- respondent_type_distribution(self$respondent_types)
        respondent_taste <- sample(types$values, n_respondents, TRUE, types$probs)
        taste <- rep(rep(respondent_taste, each = self$n_tasks), each = 2)
        eta <- profile_utility(prof_df, self$levels, self$amces, self$interactions, taste)
        eta_pair <- matrix(eta, ncol = 2, byrow = TRUE)
        p_first <- plogis(eta_pair[, 1] - eta_pair[, 2])
        win <- ifelse(runif(n_pairs) < p_first, 1L, 2L)
      }

      chosen <- integer(n_rows)
      chosen[(2 * seq_len(n_pairs) - 2) + win] <- 1

      out <- bind_cols(
        prof_df,
        chosen  = chosen,
        pair_id = rep(pair_ids, each = 2),
        resp_id = rep(rep(resp_ids, each = self$n_tasks), each = 2),
        alt_id  = rep.int(1:2, times = n_pairs)
      )

      out$p <- Reduce(`*`, Map(function(a) prob_lookup[[a]][out[[a]]], attrs))

      return(out)
    },

    simulate_conjoint = function(alpha = 0.05, chunk_size = 100, experiment_size = 25000, weight_fn = NULL) {
      cj_data <- tibble()
      cj_estimates <- tibble()

      for (i in seq(0, experiment_size, by = chunk_size)[-1]) {
        cj_data <- bind_rows(cj_data, self$sample(n_respondents = chunk_size)) |>
          mutate(
            Party = factor(Party, levels = level_values(self$levels[["Party"]])),
            Region = factor(Region, levels = level_values(self$levels[["Region"]]))
          )
        # Estimate AMCEs with cluster-robust SEs
        if (is.null(weight_fn)) {
          cj_model <- feols(chosen ~ Party + Region, data = cj_data, cluster = ~ resp_id)
        } else {
          cj_model <- feols(
            chosen ~ Party + Region,
            data = cj_data,
            cluster = ~ resp_id,
            weights = weight_fn(cj_data$p)
          )
        }

        cj_tidy <- av_tidy(
          cj_model,
          alpha = alpha,
          cluster = cj_data$resp_id
        ) |>
          mutate(i = i) |>
          filter(term != "(Intercept)")
        cj_estimates <- bind_rows(cj_estimates, cj_tidy)

        self$probabilities <- bind_rows(
          self$probabilities,
          mutate(
            tidyr::unnest_longer(tibble::enframe(self$levels), col = "value"),
            iter = i
          )
        )
      }

      truth <- tibble(amces = compute_true_amces(
        self$levels,
        self$amces,
        self$interactions,
        dgp = self$dgp,
        respondent_types = self$respondent_types
      )) |>
        unnest_longer("amces", values_to = "amce", indices_to = "level") |>
        mutate("attribute" = c(
          rep("Party", length(level_values(self$levels[["Party"]])) - 1),
          rep("Region", length(level_values(self$levels[["Region"]])) - 1)
        ))
      cj_estimates <- cj_estimates |>
        separate("term", into = c("attribute", "level"), sep = "(?<=[a-z])(?=[A-Z])") |>
        left_join(truth, by = c("attribute", "level"))

      self$estimates <- cj_estimates
      cj_estimates
    },

    simulate_conjoint_fixed = function(alpha = 0.05, experiment_size = 25000, weight_fn = NULL) {
      # Sample data
      cj_data <- self$sample(n_respondents = experiment_size) |>
        mutate(
          Party = factor(Party, levels = level_values(self$levels[["Party"]])),
          Region = factor(Region, levels = level_values(self$levels[["Region"]]))
        )
      # Fit model
      if (is.null(weight_fn)) {
        cj_model <- feols(chosen ~ Party + Region, data = cj_data, cluster = ~ resp_id)
      } else {
        cj_model <- feols(
          chosen ~ Party + Region,
          data = cj_data,
          cluster = ~ resp_id,
          weights = weight_fn(cj_data$p)
        )
      }
      # Tidy the results up
      cj_tidy <- broom::tidy(cj_model, conf.int = TRUE) |> filter(term != "(Intercept)")
      self$probabilities <- tidyr::unnest_longer(tibble::enframe(self$levels), col = "value")
      # Append the true estimand values
      truth <- tibble(amces = compute_true_amces(
        self$levels,
        self$amces,
        self$interactions,
        dgp = self$dgp,
        respondent_types = self$respondent_types
      )) |>
        unnest_longer("amces", values_to = "amce", indices_to = "level") |>
        mutate("attribute" = c(
          rep("Party", length(level_values(self$levels[["Party"]])) - 1),
          rep("Region", length(level_values(self$levels[["Region"]])) - 1)
        ))
      cj_tidy <- cj_tidy |>
        separate("term", into = c("attribute", "level"), sep = "(?<=[a-z])(?=[A-Z])") |>
        left_join(truth, by = c("attribute", "level"))
      # Save results and return them
      self$estimates <- cj_tidy
      cj_tidy
    },

    power = function(
      n_sim = 100,
      alpha = 0.05, 
      chunk_size = 100, 
      experiment_size = 2000,
      parallel = TRUE,
      verbose = TRUE,
      n_workers = NULL
    ) {
      power_calc <- function() {
        if (!is.null(n_workers)) {
          if (parallel) plan(multicore, workers = n_workers)
        } else {
          if (parallel) plan(multicore)
        }
        sims <- future_lapply(
          1:n_sim,
          function(sim_iter) {
            sim <- self$simulate_conjoint(
              alpha = alpha,
              chunk_size = chunk_size,
              experiment_size = experiment_size
            )
            sim <- mutate(sim, sim_iter = sim_iter)
            return(sim)
          },
          future.seed = TRUE
        )
        sims <- bind_rows(sims)
        if (parallel) plan(sequential)
        return(sims)
      }
      sim_results <- tryCatch({
        retry({power_calc()}, n = 5) 
      }, error = function(e) retry({power_calc()}, n = 5))
      return(sim_results)
    },

    power_fixed = function(
      n_sim = 100,
      alpha = 0.05,
      experiment_size = 2000,
      parallel = TRUE,
      verbose = TRUE,
      n_workers = NULL
    ) {
      power_calc <- function() {
        if (!is.null(n_workers)) {
          if (parallel) plan(multicore, workers = n_workers)
        } else {
          if (parallel) plan(multicore)
        }
        sims <- future_lapply(
          1:n_sim,
          function(sim_iter) {
            sim <- self$simulate_conjoint_fixed(
              alpha = alpha,
              experiment_size = experiment_size
            )
            sim <- mutate(sim, sim_iter = sim_iter)
            return(sim)
          },
          future.seed = TRUE
        )
        sims <- bind_rows(sims)
        if (parallel) plan(sequential)
        return(sims)
      }
      sim_results <- tryCatch({
        retry({power_calc()}, n = 5) 
      }, error = function(e) retry({power_calc()}, n = 5))
      return(sim_results)
    },

    p_init = NULL,
    p_target = NULL,
    sig_counter = NULL
  )
)

# This function is used to calculate exact design-based AMCE values.
compute_true_amces <- function(
  levels,
  amces,
  interactions = NULL,
  dgp = "logit",
  respondent_types = c("-0.45" = 0.25, "0" = 0.50, "0.45" = 0.25)
) {
  dgp <- match.arg(dgp, c("logit", "linear"))
  if (dgp == "linear") {
    compute_linear_amces(levels, amces, interactions)
  } else {
    compute_logit_amces(levels, amces, interactions, respondent_types)
  }
}

# A simple function to retry an expression if it fails
retry <- function(expr, n = 3, silent = TRUE) {
  expr_sub <- substitute(expr)
  for (i in seq_len(n)) {
    result <- try(eval(expr_sub, envir = parent.frame()), silent = silent)
    if (!inherits(result, "try-error")) {
      return(result)
    }
  }
  stop(sprintf("Expression failed after %d attempts.", n))
}
