% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datasim.R
\name{sample_strat_data}
\alias{sample_strat_data}
\title{Function for sampling from some distributions given stratum sizes}
\usage{
sample_strat_data(stratum_sizes, sampling_fun = sample_rgamma, params, ...)
}
\arguments{
\item{stratum_sizes}{Vector of length J indicating the size of each stratum.}

\item{sampling_fun}{Function which takes as input a sampling size and a list of
distribution parameters. By default samples from a Gamma distribution.}

\item{params}{List of length J of distribution parameters to be passed to \code{sampling_fun}. If this
is \code{sample_rgamma} (default), each element should contain shape and rate parameters.}

\item{...}{Additional functions to be passed to \code{sampling_fun}}
}
\value{
List containing a vector of the data and a vector indicating the stratum to
which each data point belongs.
}
\description{
Function for sampling from some distributions given stratum sizes
}
