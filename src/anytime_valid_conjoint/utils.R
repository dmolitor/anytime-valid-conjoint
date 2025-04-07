library(tibble)
library(tidyr)

add_reference <- function(data, low = "conf.low", high = "conf.high") {
  data <- data |>
    separate(col = "contrast", into = c("contrast", "reference"), sep = " - ")
  data <- bind_rows(
    data,
    data |>
      mutate(contrast = reference) |>
      distinct(term, contrast, .keep_all = TRUE) |>
      mutate(across(c(estimate, {{low}}, {{high}}), ~ 0))
  )
  return(data)
}

# Helper functions
projection_matrix <- function(X) {
  QRmat <- qr(X)
  Q <- qr.Q(QRmat)
  tcrossprod(Q)
}

MPxM <- function(M, X) {
  QRmat <- qr(X)
  Q <- qr.Q(QRmat)
  QtM <- crossprod(Q, M)
  crossprod(QtM)
}

MP1M <- function(M) {
  n <- nrow(M)
  if (is.null(n)) {
    M <- matrix(M, ncol = 1)
  }
  Mbar <- colSums(M)
  tcrossprod(Mbar) / n
}

# Sequential t-test: log Bayes factor for the univariate case
log_bf <- function(t2, nu, phi, z2) {
  r <- phi / (phi + z2)
  0.5 * log(r) + 0.5 * (nu + 1) * (log(1 + t2 / nu) - log(1 + r * t2 / nu))
}

# Multivariate sequential F-test: log Bayes factor
log_bf_multivariate <- function(delta, n, p, d, Phi, ZtZ, s2) {
  if (d > 1) {
    normalizing_constant <- 0.5 * log(det(Phi)) - 0.5 * log(det(Phi + ZtZ))
  } else {
    normalizing_constant <- 0.5 * log(Phi) - 0.5 * log(Phi + ZtZ)
  }
  sol <- if (d > 1) {
    solve(ZtZ + Phi, ZtZ)
  } else {
    ZtZ / (ZtZ + Phi)
  }
  normalizing_constant +
    0.5 * (n - p) * (
      log(1 + t(delta) %*% ZtZ %*% delta / (s2 * (n - p - d))) -
      log(1 + t(delta) %*% (ZtZ - ZtZ %*% sol) %*% delta / (s2 * (n - p - d)))
    )
}

# Calculate the p-value from a sequential t-test
# 
# This function will calculate a sequential t-test p-value for an arbitrary
# point estimate provided that the given standard error (SE) is valid
sequential_t_p_value <- function(estimate, se, df, phi = 1) {
  # Compute the t-statistic
  t_val <- estimate / se
  # Compute "design factor" reciprocal: z2 = sigma2/(se^2)
  z2 <- 1 / (se^2)
  # Compute the log Bayes factor (assumes log_bf() is defined)
  bf_log <- log_bf(t_val^2, df, phi, z2)
  # Sequential p-value is 1/B_n = exp(-bf_log), capped at 1
  pval <- min(1, exp(-bf_log))
  list(
    t_value = t_val,
    p_value = pval
  )
}

# Calculate an alpha-level confidence sequence (CS) from a sequential t-test
# 
# This function will calculate a sequential t-test CS for an arbitrary
# point estimate provided that the given SE is valid
sequential_t_cs <- function(estimate, se, df, alpha = 0.05, phi = 1) {
  # For an arbitrary estimand (e.g. a marginal effect), the user must supply:
  # - estimate: the point estimate (e.g., the marginal effect)
  # - se: its standard error (which already reflects all sources of uncertainty)
  # - df: degrees of freedom (for a linear model, typically the
  #   residual df = n - p, including the intercept);
  #   see `insight::get_df(..., type = "wald")`
  # - phi: a tuning parameter (default 1; for instance, phi = 1/MDE^2 if desired)

  # Compute the t-statistic
  t_val <- estimate / se
  # In the linear model context, se^2 = sigma2 * d,
  # so z2 = sigma2 / se^2 is the reciprocal of the design factor d.
  z2 <- 1 / (se^2)
  # Compute r = phi/(phi + z2)
  r <- phi / (phi + z2)
  # To avoid division by zero, set a small epsilon
  eps <- 1e-8
  term <- (r * alpha^2)^(1 / (df + 1))
  numerator <- 1 - term
  denominator <- max(term - r, eps)
  # The confidence sequence radius
  radii <- se * sqrt(df * (numerator / denominator))
  lower <- estimate - radii
  upper <- estimate + radii
  # Sequential p-value (using the log Bayes factor; assumes log_bf() is defined)
  p_value <- min(1, exp(-log_bf(t_val^2, df, phi, z2)))
  list(
    t_value = t_val,
    p_value = p_value,
    cs_lower = lower,
    cs_upper = upper
  )
}

sequential_f_p_value <- function(delta, n, n_params, Z, phi = 1) {
  # delta: vector of estimates for the parameters of interest (length d)
  # n: sample size
  # n_covar: total number of covariates in the model (including intercept)
  # ZtZ: the d x d matrix (inverse of the vcov matrix for delta)
  # phi: tuning parameter
  d <- length(delta)
  # Calculate the number of nuisance parameters
  p <- n_params - d
  Phi <- diag(d) * phi
  pval <- min(1, exp(-log_bf_multivariate(delta, n, p, d, Phi, Z, 1)))
  return(pval)
}

sequential_f_cs <- function(delta, se, n, n_params, Z, alpha = 0.05, phi = 1, term = NULL, contrast = NULL) {
  # delta: vector of estimates (e.g., marginal effects) for parameters of interest (length d)
  # se: corresponding standard errors for these estimates
  # n: total sample size
  # n_covar: total number of covariates in the full model (including intercept)
  # Z: the d x d matrix obtained by inverting the variance-covariance matrix 
  #       for delta (i.e., ZtZ = solve(vcov_delta))
  # alpha: significance level (default 0.05)
  # phi: tuning parameter (default 1; e.g., 1/MDE^2 if you have a target MDE)
  # feature_names: optional names for the estimates; if not provided, uses names(delta) or defaults

  # Number of parameters of interest
  d <- length(delta)
  # Use provided names if available; otherwise generate defaults
  if (is.null(term)) {
    term <- if (!is.null(names(delta))) names(delta) else paste0("X", 1:d)
  }
  # Compute the number of nuisance parameters:
  # Here we assume the full model has n_covar parameters.
  # Since delta covers d parameters, nuisance parameters = n_covar - d.
  p <- n_params - d
  # Degrees of freedom: using the full model's residual df approximation:
  # In a standard linear model, df = n - p - 1 (with intercept counted in p)
  nu <- n - p - 1
  # Compute individual t-statistics
  tstats <- delta / se
  tstats2 <- tstats^2
  # Compute z2 = s2 / (se^2). In a linear model, se^2 = s2 * (design factor),
  # so z2 is the reciprocal of that design factor.
  z2 <- 1 / (se^2)
  # Compute the individual sequential p-values using the univariate log Bayes factor.
  # (Assumes the function log_bf() is defined as in the paper.)
  spvalues <- pmin(1, exp(-log_bf(tstats2, nu, phi, z2)))
  # Compute confidence sequence radii for each parameter:
  # r is defined as phi / (phi + z2)
  r <- phi / (phi + z2)
  eps <- 1e-8  # safeguard against division by zero
  alpha_factor <- (r * alpha^2)^(1 / (nu + 1))
  numerator <- 1 - alpha_factor
  denominator <- pmax(alpha_factor - r, eps)
  radii <- se * sqrt(nu * (numerator / denominator))
  cs_lower <- delta - radii
  cs_upper <- delta + radii
  # Compute the multivariate sequential F-test p-value over the parameters of interest.
  # This tests the joint null that all delta = 0.
  f_seq_p <- sequential_f_p_value(delta, n, n_params, Z, phi)
  # Construct a data frame with the results
  result_df <- tibble(
    term = term,
    contrast = contrast,
    estimate = delta,
    std_error = se,
    t_stat = tstats,
    p_value = spvalues,
    cs_lower = cs_lower,
    cs_upper = cs_upper,
    f_p_value = f_seq_p
  )
  return(result_df)
}

sequential_asymptotic_cs <- function(delta, n, propensity, lambda = 100, alpha = 0.05, sigma_hat = NULL, term = NULL, contrast = NULL) {
  # delta: vector of ATE estimates (e.g., marginal effects for treatment) 
  # n: total sample size
  # propensity: treatment assignment probability (e.g., mean of a 0/1 treatment variable)
  # lambda: tuning parameter (e.g., based on your minimum detectable effect)
  # alpha: significance level (default 0.05)
  # sigma_hat: estimated residual standard error; see `insight::get_sigma()`
  # term: optional names for the estimates; if not provided, uses names(delta) or defaults like "ATE_1", "ATE_2", etc.
  # contrast: optional contrast information
  
  # If sigma_hat is not provided, assume the SE is fully adjusted and set sigma_hat to 1.
  if (is.null(sigma_hat)) sigma_hat <- 1
  # Use provided feature names if available; otherwise generate defaults.
  if (is.null(term)) {
    term <- if (!is.null(names(delta))) names(delta) else paste0("ATE_", seq_along(delta))
  }
  # Compute the half-width (r_n) using the asymptotic formula from Theorem 6.1:
  #   r_n = sigma_hat / sqrt(n * propensity * (1 - propensity)) * sqrt(((lambda + n) / n) * log((lambda + n) / (lambda * alpha^2)))
  half_width <- sigma_hat / sqrt(n * propensity * (1 - propensity)) * 
                sqrt(((lambda + n) / n) * log((lambda + n) / (lambda * alpha^2)))
  # Construct the confidence sequence for each ATE estimate.
  cs_lower <- delta - half_width
  cs_upper <- delta + half_width
  # Return a tibble with the results.
  result_df <- tibble(
    term = term,
    contrast = contrast,
    estimate = delta,
    half_width = half_width,
    cs_lower = cs_lower,
    cs_upper = cs_upper,
    n = n,
    propensity = propensity,
    lambda = lambda,
    alpha = alpha,
    sigma_hat = sigma_hat
  )
  return(result_df)
}
