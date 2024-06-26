% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_cfa_estimates.R
\name{calculate_cfa_est}
\alias{calculate_cfa_est}
\title{Calculate Confirmatory Factor Analysis Estimates}
\usage{
calculate_cfa_est(u, estimates)
}
\arguments{
\item{u}{A character string of the latent variable name.}

\item{estimates}{The estimates data frame of a confirmatory factor analysis
(CFA) \code{lavaan} summary object.}
}
\value{
A data frame of the latent variable estimates, reliability, and
  validity.
}
\description{
Calculate the latent variable estimates, reliability, and
  validity.
}
