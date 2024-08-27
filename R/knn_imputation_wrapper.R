#' KNN Imputation Wrapper
#' 
#' @description
#' A function to automate k nearest neighbor (KNN) imputation for missing
#' data. Imputing the dataframe with both the predictor and outcome variables
#' can inflate the association between the two batteries because the algorithm
#' is using information from the predictors to impute the outcome, and vice
#' versa. To prevent this, the outcome and predictor variables should be
#' imputed independently, or with a third, unrelated battery of variables.
#' This function automates this process and returns an imputed dataframe where
#' the outcome and predictor variables are imputed independently. The KNN
#' imputation process is done through the \code{VIM} package's \code{kNN()}
#' function.
#' 
#' @param data A dataframe object. This should be a structured dataset where
#' each column represents a variable and each row represents an observation.
#' @param outcomes An optional vector of column names representing the 
#' outcome variables. Specify this argument to separate the outcome variables
#' from the predictor variables when imputing. If not specified, the entire
#' dataframe is imputed together. Default is NULL.
#' @param demographics An optional vector of column names representing the
#' demographic variables. Specify this argument to leverage demographic or
#' other variables to impute missing data. If any of the demographic variables
#' data are also missing, then the demographic variables are imputed twice,
#' once with the predictor variables and once with the outcome variables. Only
#' the imputed data for the predictor variables are kept in the returned
#' dataset. Default is NULL.
#' @param id An optional character string indicating the response ID variable
#' to be used for merging imputed outcome and predictor variables. `id` Must
#' be specified if `outcomes` is specified.
#' @param k The number of nearest neighbors to use when imputing data. Default
#' is 10.
#' 
#' @returns An imputed dataframe.
#' 
#' @references 
#' Alexander Kowarik, Matthias Templ (2016). Imputation with the R
#' Package VIM. \emph{Journal of Statistical Software}, \emph{74}(7), 1-16.
#' doi:10.18637/jss.v074.i07
#' 
#' @export
knn_impute <- function(data = .,
                       outcomes = NULL,
                       demographics = NULL,
                       id = NULL,
                       k = 10){
  
  if ((is.null(outcomes) & !is.null(id)) |
      (is.null(id) & !is.null(outcomes))) {
    stop("Please specify both `outcomes` and `id`.")
  }
  
  
  # If outcomes is specified
  if(!is.null(outcomes)){
    
    # If demographic variables are specified, include them in partitioning the
    # outcome variables. Otherwise, only grab the outcome variables. 
    if(!is.null(demographics)){
      
      outcome <- data %>% 
        dplyr::select(all_of(id),
                      all_of(demographics),
                      all_of(outcomes))
      
      subdata <- data %>% 
        dplyr::select(-all_of(outcomes))
      
    } else {
      
      outcome <- data %>% 
        dplyr::select(all_of(id),
                      all_of(outcomes))
      
      subdata <- data %>% 
        dplyr::select(-all_of(outcomes))
      
    }
    
    # Impute data
    knn_sub <- VIM::kNN(subdata, k = k, imp_var = F)
    knn_out <- VIM::kNN(outcome, k = k, imp_var = F)
    
    # Merge with the id variable
    knn_data <- knn_sub %>% 
      inner_join(knn_out %>% 
                   dplyr::select(all_of(outcomes), id),
                 by = id)
    
  } else {
    
    # If outcomes not specified, then impute entire dataset
    knn_data <- VIM::kNN(data, k = k, imp_var = F)
    
  }
  
  # Return imputed dataset
  return(knn_data)
  
}