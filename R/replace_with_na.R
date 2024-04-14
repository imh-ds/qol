#' Replace values with NA
#' 
#' @description
#' A simple function to replace values of specific variables with NA. This
#' function is meant to mimic the \code{naniar} package's
#' \code{replace_with_na()} function expedite the processing speed. For more
#' complex functionality around dealing with missing data, see Tierney and
#' Cook's (2023) \code{naniar} package.
#' 
#' @param data A dataframe object. This should be a structured dataset where
#' each column represents a variable and each row represents an observation.
#' @param variables A required vector of column names representing their 
#' manifest variables.
#' @param values A required vector of numeric or string values to be replaced
#' with NA.
#' 
#' @return A dataframe of the dataset with the values of the variables replaced
#' with NA (missing).
#' 
#' @examples
#' replace_with_na(data,
#'                 variables = c("var1", "var2"),
#'                 values = c(99, 98))
#' replace_with_na(data,
#'                 variables = c("educ"),
#'                 values = c("Unknown", "Prefer not to say"))
#' 
#' @references
#' Tierney N, Cook D (2023). “Expanding Tidy Data Principles to Facilitate
#' Missing Data Exploration, Visualization and Assessment of Imputations.”
#' \emph{Journal of Statistical Software}, \emph{105}(7), 1-31.
#' doi:10.18637/jss.v105.i07.
#' 
#' @export
replace_with_na <- function(data = .,
                            variables,
                            values){
  
  # Change values in variables to NA
  data <- data %>% 
    dplyr::mutate(dplyr::across(.cols = dplyr::all_of(variables),
                                ~replace(., . %in% values, NA)))
  
  # Return data
  return(data)
  
}
