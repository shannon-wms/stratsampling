#' Functions for simulating data

#' Sample from a Gamma distribution.
sample_rgamma <- function(size, params) {
  rgamma(size, params[[1]], params[[2]])
}

#' Sample stratum weights randomly given a number of strata
#'
#' @param n_strata Number of strata.
#' @param shape Shape parameter of Gamma distribution with rate 1 used for generating
#' random weights. Defaults to 5.
#'
#' @return
#' @export
sample_weights <- function(n_strata, shape = 5) {
  ys <- rgamma(n_strata, shape, 1)
  weights <- sort(ys / sum(ys), decreasing = TRUE)
  return(weights)
}


#' Sample exceedances via sampling from a Gamma distribution (to simulate an ash
#' concentration curve) and checking whether samples exceed a chosen threshold.
#'
#' @param N Sample size.
#' @param params List containing shape and rate parameter for Gamma distribution.
#' @param threshold Threshold for evaluating threshold.
#'
#' @return Binary vector of length `N`, where 1 indicates exceedance and 0 no exceedance.
#' @export
sample_exc_data <- function(N, params, threshold) {
  data <- rgamma(size, params[[1]], params[[2]])
  exc_data <- rep(0, N)
  exc_data[which(threshold <= data)] <- 1
  return(exc_data)
}

#' Function for sampling from some distributions given stratum sizes
#'
#' @param stratum_sizes Vector of length J indicating the size of each stratum.
#' @param sampling_fun Function which takes as input a sampling size and a list of
#' distribution parameters. By default samples from a Gamma distribution.
#' @param params List of length J of distribution parameters to be passed to `sampling_fun`. If this
#' is `sample_rgamma` (default), each element should contain shape and rate parameters.
#' @param ... Additional functions to be passed to `sampling_fun`
#'
#' @return List containing a vector of the data and a vector indicating the stratum to
#' which each data point belongs.
#' @export
sample_strat_data <- function(stratum_sizes, sampling_fun = sample_rgamma,
                              params, ...) {
  n_strata <- length(weights)
  # Sample "ash concentration" data
  strata <- c()
  data <- c()
  for (i in 1:n_strata) {
    size <- stratum_sizes[i]
    strat_params <- lapply(params, `[[`, i)
    strat_data <- sampling_fun(size, strat_params, ...)

    data <- c(data, strat_data)
    strata <- c(strata, rep(i, size))
  }

  return(list(data = data, strata = strata))
}

#' For each threshold, get the number of exceedances in the data set.
#'
#' @param data Vector of data points of length N.
#' @param threshold Vector of thresholds of length M.
#'
#' @return Matrix with dimensions N*M, where entry i,j indicates whether data point i
#' exceeds threshold j.
#' @export
get_exc <- function(data, threshold) {
  N <- length(data)
  n_threshold <- length(threshold)
  threshold <- sort(threshold)

  exc_data <- matrix(0, nrow = length(data), ncol = n_threshold)
  for (i in 1:length(data)) {
    exc_data[i, threshold <= data[i]] <- 1
  }

  return(exc_data)
}

