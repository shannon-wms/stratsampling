#' Functions for stratified sampling

#' Proportional allocation
#' Allocate stratum sizes by N * w_j, where N is the total number of samples and
#' w_j the weight of stratum j = 1:J.
#'
#' @param N Total number of samples.
#' @param weights Vector of stratum weights of length J. Must sum to one.
#'
#' @return Vector of stratum sizes.
#' @export
prop_alloc <- function(N, weights) {
  if (sum(weights) != 1) stop("Sum of weights must be unity.")

  x <- weights * N
  sizes <- floor(x)
  ind <- tail(order(x - sizes), round(sum(x) - sum(sizes)))
  sizes[ind] <- sizes[ind] + 1

  return(sizes)
}

#' Get stratified estimates and variances given data, strata, and stratum weights.
#'
#' @param data Vector of length N containing data to be aggregated.
#' @param strata Vector of length N indicating the stratum to which each data point
#' belongs.
#' @param weights Vector of stratum weights of length J. Must sum to one.
#'
#' @return Named list containing stratifed sampling estimate, the variance of the
#' estimate, and stratum means, variances, and sizes.
#' @export
strat_ests <- function(data, strata, weights) {
  if (sum(weights) != 1) stop("Sum of weights must be unity.")
  if (length(data) != length(strata)) {
    stop("Length of strata must equal length of input data.")
  }
  if (length(unique(strata)) != length(weights)) {
    stop("Length of weights must equal the number of unique strata.")
  }

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

#' Get unstratified sampling estimates and its variance, i.e., stochastic sampling
#' estimates.
#'
#' @param data Vector containing data whose mean is to be estimated.
#'
#' @return Named list containing the stochastic sampling estimate and its variance.
#' @export
unstrat_ests <- function(data) {
  unstrat_est <- mean(data)
  unstrat_var_est <- var(data) / length(data) # Sample variance / no. of samples
  return(list(estimate = unstrat_est,
              variance_est = unstrat_var_est))
}

#' Get confidence interval and (optionally) apply a transformation.
#'
#' @param estimate Stochastic sampling estimate.
#' @param var_estimate Variance of stratified sampling estimate.
#' @param alpha Confidence interval level, default 0.05 to produce a 95% confidence
#' interval.
#' @param g A transformation to be applied to the data, provided as an expression of `x`.
#' @param gprime The first derivative of g, provided as an expression of `x`. If not
#' provided, will be computed using `D(g, 'x')`.
#'
#' @return Upper and lower bounds of the confidence interval. If `g` is provided,
#' returns a list containing both the untransformed and transformed confidence
#' interval.
#' @export
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

#' Get confidence intervals for a series of estimates.
#'
#' @param ests Vector of length N containing stochastic sampling estimates.
#' @param var_ests Vector of length N  corresponding estimates of variances.
#' @param g A transformation to be applied to the data, provided as an expression of `x`.
#' @param ... Additional arguments to be passed to `get_cis`.
#'
#' @return List of length N with confidence intervals for each estimate in `ests`.
get_cis_list <- function(ests, var_ests, g = NULL, ...) {
  if (length(ests) != length(var_ests)) stop("var_ests must have same length as ests.")
  n_pts <- length(ests)
  cis_list <- list()
  for (i in 1:n_pts) {
    cis_list[[i]] <- get_cis(estimate = ests[i], var_estimate = var_ests[i], g = g, ...)
  }
  return(cis_list)
}
