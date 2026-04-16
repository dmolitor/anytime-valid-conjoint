suppressPackageStartupMessages({
  library(avlm)
  library(broom)
  library(dplyr)
  library(fixest)
  library(ggplot2)
  library(fixest)
  library(future.apply)
  library(R6)
  library(tidyr)
})

# This class implements a simple conjoint object. Primarily used for the empirical simulations
ConjointSim <- R6Class(
  "ConjointSim",
  public = list(
    levels = NULL, # list: attr -> (vector of levels OR named prob vector)
    amces = NULL, # list: attr -> named numeric (non-baseline levels; baseline=0)
    interactions = NULL, # matrix: rownames = levels(attr1), colnames = levels(attr2)
    n_tasks = NULL,
    estimates = NULL,

    initialize = function(levels, amces, interactions = NULL, n_tasks = 5) {
      self$levels <- levels
      self$amces <- amces
      self$interactions <- interactions
      self$n_tasks <- as.integer(n_tasks)
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
      p <- ggplot(
          estimates,
          aes(x = i, y = estimate, ymin = conf.low, ymax = conf.high)
        ) +
        geom_line() +
        geom_ribbon(alpha = 0.2)
      if (!uniform_only) {
        p <- p + geom_hline(aes(yintercept = amce), color = "red", linetype = "dashed")
      }
      if (show_when_stat_sig) {
        p <- p + geom_vline(
          aes(xintercept = first_stat_sig),
          linetype = "dashed",
          color = "blue"
        )
      }
      p <- p + geom_hline(
        aes(yintercept = amce),
        data = tibble(amces = self$amces) |>
          unnest_longer("amces", values_to = "amce", indices_to = "level") |>
          mutate(
            "attribute" = c(
              rep("Party", length(self$amces[["Party"]])),
              rep("Region", length(self$amces[["Region"]]))
            )
          ),
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
        labs(x = "Sample size", y = "Assignment probability", color = "")
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

      # vectorized profile draws
      draw_attr <- function(a) sample(levs[[a]], n_rows, TRUE, probs[[a]])
      prof_df <- as_tibble(setNames(lapply(attrs, draw_attr), attrs))

      # m(x): sum of amces for present non-baseline levels
      m <- numeric(n_rows)
      for (a in attrs) {
        v <- self$amces[[a]]
        if (!is.null(v) && length(v)) {
          add <- v[ prof_df[[a]] ]
          add[is.na(add)] <- 0
          m <- m + add
        }
      }

      # add interaction contribution if provided
      if (!is.null(self$interactions)) {
        attr1 <- attrs[1]
        attr2 <- attrs[2]
        IA <- self$interactions
        interaction_effects <- mapply(
          function(a, b) {
            if (!a %in% rownames(IA) || !b %in% colnames(IA)) return(0)
            IA[a, b]
          },
          prof_df[[attr1]],
          prof_df[[attr2]]
        )
        m <- m + interaction_effects
      }

      # respondent and profile noises (affect variance, not expected AMCEs)
      # alpha <- rep(rnorm(n_respondents, 0, 0.005), each = 2 * self$n_tasks)
      alpha <- rep(runif(n_respondents, 0, 0.25), each = 2 * self$n_tasks)
      m <- m + alpha

      if (any(m < -0.1 | m > 0.4)) {
        stop("Score function m(x) has too large of values; shrink amces or interactions.")
      }
      # Thought process here, what is the purpose of the 2*m scaling factor?
      # Should be C + m where C is the largest possible negative score m.
      q <- 0.2 + 2 * m

      # Bernoulli per row; tie-break within pair
      y <- rbinom(n_rows, 1, q)
      Y <- matrix(y, ncol = 2, byrow = TRUE)
      ties <- (Y[,1] == Y[,2])
      win <- integer(n_pairs)
      win[!ties] <- ifelse(Y[!ties, 1] > Y[!ties, 2], 1, 2)
      win[ties]  <- sample.int(2, sum(ties), TRUE)

      chosen <- integer(n_rows)
      chosen[(2 * seq_len(n_pairs) - 2) + win] <- 1

      out <- bind_cols(
        prof_df,
        chosen  = chosen,
        pair_id = rep(seq_len(n_pairs), each = 2),
        resp_id = rep(rep(seq_len(n_respondents), each = self$n_tasks), each = 2),
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
            Party = factor(Party, levels = names(self$levels[["Party"]])),
            Region = factor(Region, levels = names(self$levels[["Region"]]))
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
          g = optimal_g(nrow(cj_data), length(coef(cj_model)), alpha),
          alpha = alpha
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

      truth <- tibble(amces = compute_true_amces(self$levels, self$amces, self$interactions)) |>
        unnest_longer("amces", values_to = "amce", indices_to = "level") |>
        mutate("attribute" = c(rep("Party", length(self$levels[["Party"]])-1), rep("Region", length(self$levels[["Region"]])-1)))
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
          Party = factor(Party, levels = names(self$levels[["Party"]])),
          Region = factor(Region, levels = names(self$levels[["Region"]]))
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
      truth <- tibble(amces = compute_true_amces(self$levels, self$amces, self$interactions)) |>
        unnest_longer("amces", values_to = "amce", indices_to = "level") |>
        mutate("attribute" = c(rep("Party", length(self$levels[["Party"]])-1), rep("Region", length(self$levels[["Region"]])-1)))
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
      verbose = TRUE
    ) {
      power_calc <- function() {
        if (parallel) plan(multicore)
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
      verbose = TRUE
    ) {
      power_calc <- function() {
        if (parallel) plan(multicore)
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

# This function is used to calculate true AMCE values
compute_true_amces <- function(levels, amces, interactions = NULL) {
  amces <- lapply(
    names(levels),
    function(x) {
      baseline <- names(levels[[x]])[[1]]
      vapply(
        names(levels[[x]][-1]),
        function(y) {
          conditioning_var <- names(levels)[!names(levels) == x]

          # Calculates E_{Conditioning Attribute}(E[Level - Baseline | Condit. Attr.])

          sum(vapply(
            names(levels[[conditioning_var]]),
            function(conditioning_level) {

              # Calculates E[Level - Baseline | Conditioning Attribute] * Pr(Cond. Attr)

              if (conditioning_level %in% colnames(interactions)) {
                pr = (
                  (amces[[x]][[y]] + interactions[y, conditioning_level]) -
                  (0 + interactions[baseline, conditioning_level])
                ) * levels[[conditioning_var]][[conditioning_level]]
              } else {
                pr = (
                  (amces[[x]][[y]] + interactions[conditioning_level, y]) -
                  (0 + interactions[conditioning_level, baseline])
                ) * levels[[conditioning_var]][[conditioning_level]]
              }
              return(pr)
            },
            numeric(1)
          ))
        },
        numeric(1)
      )
    }
  )
  names(amces) <- names(levels)
  return(amces)
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