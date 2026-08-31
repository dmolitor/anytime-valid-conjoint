suppressPackageStartupMessages({
  library(dplyr)
  library(future)
  library(future.apply)
  library(ggplot2)
  library(here)
  library(readr)
  library(tidyr)
})

source(here("code", "cj.R"))
source(here("code", "figure_style.R"))

options(future.globals.maxSize = Inf)

alpha <- 0.05
calibration_G <- 2500
lambda_scalar <- optimal_g(calibration_G, alpha)
lambda_joint <- optimal_g_multivariate(calibration_G, 3, alpha)
# A dense grid reduces discrete-monitoring overshoot while remaining computationally
# feasible. Adjacent standardized null statistics have approximate correlation rho.
rho <- 0.995
G_min <- 100
G_max <- 1e8
n_sim <- 5000
seed <- 476816

grid_ratio <- 1 / rho^2
G_grid <- unique(as.integer(round(
  G_min * grid_ratio^(0:ceiling(log(G_max / G_min) / log(grid_ratio)))
)))
G_grid <- G_grid[G_grid >= G_min & G_grid <= G_max]
if (tail(G_grid, 1) < G_max) G_grid <- c(G_grid, as.integer(G_max))
G_grid <- unique(c(5:99, G_grid))

make_cluster_types <- function(region_levels) {
  profile_grid <- expand.grid(
    Party = c("Right", "Left"),
    Region = region_levels,
    stringsAsFactors = FALSE
  )

  design_row <- function(party, region) {
    c(
      "(Intercept)" = 1,
      "PartyLeft" = as.numeric(party == "Left"),
      stats::setNames(
        as.numeric(region == region_levels[-1]),
        paste0("Region", region_levels[-1])
      )
    )
  }

  X_profiles <- t(mapply(
    design_row,
    profile_grid$Party,
    profile_grid$Region
  ))
  colnames(X_profiles) <- names(design_row("Right", region_levels[[1]]))

  pairs <- expand.grid(
    first = seq_len(nrow(X_profiles)),
    second = seq_len(nrow(X_profiles)),
    winner = 1:2
  )

  p <- ncol(X_profiles)
  H <- nrow(pairs)
  A_array <- array(0, dim = c(H, p, p))
  b_mat <- matrix(0, nrow = H, ncol = p)
  for (h in seq_len(H)) {
    X <- rbind(X_profiles[pairs$first[[h]], ], X_profiles[pairs$second[[h]], ])
    y <- if (pairs$winner[[h]] == 1) c(1, 0) else c(0, 1)
    A_array[h, , ] <- crossprod(X)
    b_mat[h, ] <- crossprod(X, y)
  }

  list(
    A_array = A_array,
    A_mat = matrix(A_array, nrow = H),
    b_mat = b_mat,
    prob = rep(1 / H, H),
    p = p,
    term_names = colnames(X_profiles),
    region_terms = paste0("Region", region_levels[-1])
  )
}

cluster_vcov <- function(A, beta, counts, types) {
  p <- types$p
  score_mat <- types$b_mat -
    types$A_mat %*% kronecker(matrix(beta, ncol = 1), diag(p))
  meat <- crossprod(score_mat, score_mat * counts)

  G <- sum(counts)
  N <- 2 * G
  ssc <- (G / (G - 1)) * ((N - 1) / (N - p))
  A_inv <- solve(A)
  ssc * A_inv %*% meat %*% A_inv
}

process_one_design <- function(types, G_grid, lambda_value) {
  counts <- integer(length(types$prob))
  previous_G <- 0L
  out <- vector("list", length(G_grid))

  for (idx in seq_along(G_grid)) {
    G <- G_grid[[idx]]
    increment <- G - previous_G
    previous_G <- G
    counts <- counts + as.integer(rmultinom(1, increment, types$prob))

    A <- matrix(as.numeric(crossprod(counts, types$A_mat)), nrow = types$p)
    b <- as.numeric(crossprod(counts, types$b_mat))

    beta <- tryCatch(solve(A, b), error = function(e) rep(NA_real_, types$p))
    if (anyNA(beta)) {
      out[[idx]] <- tibble(G = G, av = FALSE, fixed = FALSE)
      next
    }

    V <- tryCatch(cluster_vcov(A, beta, counts, types), error = function(e) NULL)
    if (is.null(V)) {
      out[[idx]] <- tibble(G = G, av = FALSE, fixed = FALSE)
      next
    }

    region_idx <- match(types$region_terms, types$term_names)
    beta_region <- beta[region_idx]
    V_region <- V[region_idx, region_idx, drop = FALSE]
    d <- length(region_idx)

    if (d == 1) {
      Q <- if (is.finite(V_region[1, 1]) && V_region[1, 1] > 0) {
        as.numeric(beta_region^2 / V_region[1, 1])
      } else {
        NA_real_
      }
      if (!is.finite(Q) || Q < 0) {
        p_av <- NA_real_
        p_fixed <- NA_real_
      } else {
        p_av <- p_G_t(log_G_t(Q, G, lambda_value))
        p_fixed <- 2 * stats::pt(sqrt(Q), df = G - 1, lower.tail = FALSE)
      }
    } else {
      Q <- tryCatch(
        as.numeric(crossprod(beta_region, solve(V_region, beta_region))),
        error = function(e) NA_real_
      )
      if (!is.finite(Q) || Q < 0) {
        p_av <- NA_real_
        p_fixed <- NA_real_
      } else {
        p_av <- p_G_t(log_G_multivariate_t(Q, G, lambda_value, d))
        p_fixed <- stats::pf(Q / d, df1 = d, df2 = G - d, lower.tail = FALSE)
      }
    }

    out[[idx]] <- tibble(
      G = G,
      av = is.finite(p_av) && p_av < alpha,
      fixed = is.finite(p_fixed) && p_fixed < alpha
    )
  }

  bind_rows(out) |>
    mutate(
      av = cumany(av),
      fixed = cumany(fixed)
    )
}

scalar_types <- make_cluster_types(c("North", "South"))
multivariate_types <- make_cluster_types(c("North", "South", "East", "West"))

simulate_one <- function(iter) {
  scalar <- process_one_design(scalar_types, G_grid, lambda_scalar) |>
    transmute(
      sim_iter = iter,
      G,
      Test = "Scalar region null",
      `Anytime-valid` = av,
      Conventional = fixed
    )

  multivariate <- process_one_design(
    multivariate_types,
    G_grid,
    lambda_joint
  ) |>
    transmute(
      sim_iter = iter,
      G,
      Test = "Multivariate region null",
      `Anytime-valid` = av,
      Conventional = fixed
    )

  bind_rows(scalar, multivariate) |>
    pivot_longer(
      c(`Anytime-valid`, Conventional),
      names_to = "Method",
      values_to = "any_false_positive"
    )
}

set.seed(seed)
future::plan(multicore)
sims <- future_lapply(seq_len(n_sim), simulate_one, future.seed = TRUE)
future::plan(sequential)

results <- bind_rows(sims) |>
  summarize(
    p_false_positive = mean(any_false_positive),
    se = sqrt(p_false_positive * (1 - p_false_positive) / n_sim),
    ci_low = pmax(0, p_false_positive - 1.96 * se),
    ci_high = pmin(1, p_false_positive + 1.96 * se),
    .by = c(Test, Method, G)
  ) |>
  mutate(
    rho = rho,
    lambda = if_else(
      Test == "Scalar region null",
      lambda_scalar,
      lambda_joint
    ),
    calibration_G = calibration_G,
    n_sim = n_sim
  )

final_summary <- results |>
  filter(G == max(G)) |>
  arrange(Test, Method)

write_csv(results, here("data", "figure2_long_horizon.csv"))
write_csv(final_summary, here("data", "figure2_long_horizon_final.csv"))

p <- ggplot(results, aes(x = G, y = p_false_positive, color = Method, fill = Method)) +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high), alpha = 0.12, color = NA) +
  geom_line(linewidth = 0.6) +
  geom_hline(yintercept = alpha, linetype = "dashed", color = paper_colors$reference) +
  scale_x_log10(
    labels = scales::label_comma(),
    expand = expansion(mult = c(0.03, 0.08))
  ) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1), limits = c(0, NA)) +
  scale_color_manual(values = method_palette) +
  scale_fill_manual(values = method_palette) +
  facet_wrap(~ Test) +
  labs(
    x = "Respondent clusters (G, log scale)",
    y = "Cumulative Type I error",
    color = NULL,
    fill = NULL
  ) +
  conjoint_theme()

save_paper_figure("figure2_long_horizon.png", p, width = 8, height = 4.8)

print(final_summary, n = Inf)
