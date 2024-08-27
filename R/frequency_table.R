#' Frequency Table
#'
#' @description A quick function to get a frequency table of a variable's
#'   values.
#'
#' @param data A dataframe object. This should be a structured dataset where
#'   each column represents a variable and each row represents an observation.
#' @param vars A vector of character strings representing the names of variables
#'   to create frequency tables of.
#'
#' @returns A frequency table of the variable's unique values.
#'
#' @examples
#' freq(data, "education")
#'
#' @export
freq <- function(data = .,
                 vars){
  
  # If only 1 variable
  if(length(vars) == 1){
    
    # Get frequency count
    frequency <- data %>% 
      dplyr::group_by(!!rlang::sym(vars)) %>% 
      dplyr::count()
    
  }
  
  # If more than 1 variable
  if(length(vars) > 1){
    
    frequency <- list()
    
    for(i in vars){
      
      # Get frequency count
      fr <- data %>% 
        dplyr::group_by(!!rlang::sym(i)) %>% 
        dplyr::count()
      
      # Save to list
      frequency[[i]] <- fr
      
    }
    
  }
  
  # Return
  return(frequency)
  
}
