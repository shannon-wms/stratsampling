### SIMULATING DATA ###

# Sample stratum weights randomly given a number of strata
sample_weights <- function(n_strata, shape = 5) {
  ys <- rgamma(n_strata, shape, 1)
  weights <- sort(ys / sum(ys), decreasing = TRUE)
  return(weights)
}

# Sample from a Gamma distribution (with shape <1 looks like ash concentration curves)
sample_rgamma <- function(size, params) {
  rgamma(size, shape = params[[1]], rate = params[[2]])
}

# Sample exceedances via sampling from a Gamma distribution and checking whether samples
# exceed a chosen threshold
sample_exc_data <- function(size, params, threshold) {
  data <- sample_rgamma(size, params)
  exc_data <- rep(0, size)
  exc_data[which(threshold <= data)] <- 1
  return(exc_data)
}

# Function for sampling from some distributions given stratum sizes
sample_strat_data <- function(N, stratum_sizes, sampling_fun = sample_rgamma,
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

# For each threshold, get the number of exceedances
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
