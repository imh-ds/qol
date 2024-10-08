% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tool_frequency_table.R
\name{freq}
\alias{freq}
\title{Frequency Table}
\usage{
freq(data = ., vars)
}
\arguments{
\item{data}{A dataframe object. This should be a structured dataset where
each column represents a variable and each row represents an observation.}

\item{vars}{A vector of character strings representing the names of variables
to create frequency tables of.}
}
\value{
A frequency table of the variable's unique values.
}
\description{
A quick function to get a frequency table of a variable's
  values.
}
\examples{
freq(data, "education")

}
