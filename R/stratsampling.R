### GENERAL FUNCTIONS ###

# Proportional allocation
prop_alloc <- function(N, weights) {
  x <- weights * N
  sizes <- floor(x)
  ind <- tail(order(x - sizes), round(sum(x) - sum(sizes)))
  sizes[ind] <- sizes[ind] + 1

  return(sizes)
}

# Get unstratified and stratified estimates and variances given data, strata, and
# stratum weights
strat_ests <- function(data, strata, weights) {
  n_strata <- length(weights)

  stratum_sizes <- c()
  stratum_means <- c()
  stratum_vars <- c() # Stratum sample variances

  for (i in 1:n_strata) {
    stratum_data <- data[strata == i]
    stratum_sizes[i] <- length(stratum_data)
    stratum_means[i] <- mean(stratum_data)
    stratum_vars[i] <- var(stratum_data)
  }

  strat_est <- sum(weights * stratum_means) # Stratifed sampling estimate
  strat_var_est <- sum(weights^2 * stratum_vars / stratum_sizes) # Variance of estimate

  out <- list(estimate = strat_est,
              variance_est = strat_var_est,
              stratum_means = stratum_means,
              stratum_vars = stratum_vars,
              stratum_sizes = stratum_sizes)
  return(out)
}

# Get unstratified sampling estimates
unstrat_ests <- function(data) {
  unstrat_est <- mean(data)
  unstrat_var_est <- var(data) / length(data) # Sample variance / no. of samples
  return(list(estimate = unstrat_est, variance_est = unstrat_var_est))
}

# Get confidence interval and optionally apply a transformation
get_cis <- function(estimate, var_estimate, alpha = 0.05, g = NULL, gprime = NULL) {
  # Untransformed CI
  z <- qnorm(1 - alpha / 2)
  coef <- z * sqrt(var_estimate)
  ci <- c(estimate - coef, estimate + coef)

  if (!is.expression(g)) return(ci)
  else { # Transformed CI
    x <- estimate
    g_est <- eval(g)
    if (!is.expression(gprime)) gprime <- D(g, 'x') # Get first derivative
    gp_est <- eval(gprime)
    g_coef <- z * sqrt(var_estimate * gp_est^2)
    if (!is.nan(g_coef)) g_ci <- c(g_est - g_coef, g_est + g_coef)
    else g_ci <- c(NA, NA)

    return(list(ci = ci, g_ci = g_ci))
  }
}

# Get confidence intervals for a series of estimates
get_cis_list <- function(ests, var_ests, g = NULL, ...) {
  n_pts <- length(ests)
  cis_list <- list()
  for (i in 1:n_pts) {
    cis_list[[i]] <- get_cis(estimate = ests[i], var_estimate = var_ests[i], g = g, ...)
  }
  return(cis_list)
}
