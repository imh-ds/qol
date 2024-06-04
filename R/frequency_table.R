#' Frequency Table
#' 
#' @description
#' A quick function to get a frequency table of a variable's values.
#' 
#' @param data A dataframe object. This should be a structured dataset where
#' each column represents a variable and each row represents an observation.
#' @param variable A required single string denoting the name of the variable.
#' 
#' @returns A frequency table of the variable's unique values.
#' 
#' @examples
#' freq(data, "education")
#' 
#' @export
freq <- function(data = .,
                 variables){
  
  # If only 1 variable
  if(length(variables) == 1){
    
    # Get frequency count
    frequency <- data %>% 
      dplyr::group_by(!!rlang::sym(variables)) %>% 
      dplyr::count()
    
  }
  
  # If more than 1 variable
  if(length(variables) > 1){
    
    frequency <- list()
    
    for(i in variables){
      
      # Get frequency count
      fr <- data %>% 
        dplyr::group_by(!!rlang::sym(i)) %>% 
        dplyr::count()
      
      # Save to list
      frequency[[i]] <- fr
      
    }
    
  }
  
  
  return(frequency)
  
}
