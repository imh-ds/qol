% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_independent_ttest.R
\name{calc_ind_ttest}
\alias{calc_ind_ttest}
\title{Calculate Independent Samples t-test}
\usage{
calc_ind_ttest(
  data = .,
  outcome,
  group,
  alternative = "two.sided",
  var_equal = FALSE,
  conf_level = 0.95,
  es_type = "hedges",
  g_correction = "standard"
)
}
\arguments{
\item{data}{A dataframe object. This should be a structured dataset where
each column represents a variable and each row represents an observation.}

\item{outcome}{Character indicating the outcome/dependent variable.}

\item{group}{Character indicating the binary factor grouping variable.}

\item{alternative}{Character indicating the direction of the alternative
hypothesis. Options include \code{"two.sided"}, \code{"greater"}, or
\code{"less"}. The default is two sided alternative hypothesis.}

\item{var_equal}{Logical indicating whether the groups' variances are equal
or not. If \code{TRUE}, standard Student's t-test is run with the pooled
variance. If \code{FALSE}, Welch's t-test approximation to the degrees of
freedom is run.}

\item{conf_level}{Numeric value reflecting the confidence interval level. The
default is 0.95 for 95% confidence interval.}

\item{es_type}{Character indicating which effect size to return. Options
include \code{"cohen"} for Cohen's d, \code{"hedges"} for Hedges' g, or
\code{"glass"} for Glass's delta. The default is Hedges' g.}

\item{g_correction}{Character indicating which Hedges' g correction to apply.
Options include \code{"standard"} for regular Hedges' g correction and
\code{"log"} to apply logarithmic Hedges' g correction. The default is
standard.}
}
\description{
Calculate Student's or Welch's independent samples t-test.
}
