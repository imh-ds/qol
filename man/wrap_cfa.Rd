% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wrapper_confirmatory_factor_analysis.R
\name{wrap_cfa}
\alias{wrap_cfa}
\title{Confirmatory Factor Analysis Wrapper}
\usage{
wrap_cfa(
  data = .,
  model,
  name = NULL,
  cluster = NULL,
  missing = "listwise",
  se = NULL,
  bootstrap = 1000,
  estimator = "ML",
  digits = 3,
  standardized = TRUE,
  mi_groups = NULL
)
}
\arguments{
\item{data}{A data frame. Each column represents a variable and each row an
observation.}

\item{model}{A CFA model compatible with \code{lavaan}. See \code{lavaan}
documentation for instructions on specifying a CFA model.}

\item{name}{An optional name for the CFA model. Defaults to 'CFA Model' if
not specified.}

\item{cluster}{An optional character argument. A variable name defining
clusters in a multi-level dataset.}

\item{missing}{Method for handling missing data. Default is "listwise", which
deletes all observations with missing values prior to analysis. This is
recommended when data are missing completely at random (MCAR). Another
option is "ml" for full information maximum likelihood (FIML). See
\code{lavaan} documentation for more details.}

\item{se}{Method for computing standard errors. Default is NULL. Set to
"robust" to use either "robust.sem" or "robust.huber.white" depending on
the estimator used. See \code{lavaan} documentation for more details.}

\item{bootstrap}{An integer specifying the number of bootstrap iterations. A
higher value provides more robust results but increases computational time.
Default is 1000.}

\item{estimator}{Estimator for the CFA. Default is maximum likelihood (ML).
Other estimators are those offered by \code{lavaan}, e.g., \code{"GLS"},
\code{"WLS"}, \code{"DWLS"}, \code{"ULS"}, \code{"DLS"}, and \code{"PML"}.
See \code{lavaan} documentation for more details.}

\item{digits}{An integer specifying the number of decimal places for rounding
in the in-text reference generator.}

\item{standardized}{A logical value indicating whether to return standardized
or unstandardized estimates. If \code{TRUE}, the function returns
standardized estimates. If \code{FALSE}, it returns unstandardized
estimates. Default is \code{TRUE}.}

\item{mi_groups}{A vector of column names representing the categorical group
variables for testing measurement invariance. Default is \code{NULL} which
means measurement invariance is not run. If specified, the function will
run measurement invariance on the groups and return fit metrics.}
}
\value{
A list containing a \code{lavaan} CFA model and data frames with the
  results of the analysis. The basic output includes data frame tables for
  goodness of fit metrics and model estimations. If measurement invariance
  groups were specified, fit indices and goodness of fit comparison tables
  are also included in the output.
}
\description{
This function conducts a confirmatory factor analysis (CFA)
  using the \code{lavaan} package (Rosseel, 2012). It automatically compiles
  and returns tables of commonly reported results. The function supports both
  regular CFA and measurement invariance (MI).
}
\examples{
model <- 'anxiety =~ x1 + x2 + x3
          sleep   =~ z1 + z2'
cfa_wrapper(data, model, mi_groups = c("gender", "ethnicity"))


}
\references{
Yves Rosseel (2012). lavaan: An R Package for Structural Equation
  Modeling. \emph{Journal of Statistical Software}, 48(2), 1-36.
  \url{https://doi.org/10.18637/jss.v048.i02}.
}
