% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stratsampling.R
\name{get_cis_list}
\alias{get_cis_list}
\title{Get confidence intervals for a series of estimates.}
\usage{
get_cis_list(ests, var_ests, g = NULL, ...)
}
\arguments{
\item{ests}{Vector of length N containing stochastic sampling estimates.}

\item{var_ests}{Vector of length N  corresponding estimates of variances.}

\item{g}{A transformation to be applied to the data, provided as an expression of \code{x}.}

\item{...}{Additional arguments to be passed to \code{get_cis}.}
}
\value{
List of length N with confidence intervals for each estimate in \code{ests}.
}
\description{
Get confidence intervals for a series of estimates.
}
