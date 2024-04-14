#' Split Data Into Train, Validation, and Test Sets
#' 
#' @description
#' Automated wrapper function to split a dataframe into training, validation,
#' and testing sets for machine learning or cross-validation. Validation and
#' test sets are equally split from the remaining dataframe after subsetting
#' the training set. Training, validation, and test sets are saved as
#' dataframes in a returned list.
#' 
#' @param data A dataframe object. This should be a structured dataset where
#' each column represents a variable and each row represents an observation.
#' @param group If there are subgroups in the dataset that also need to be
#' equally split into training, validation, or test sets, specify the column
#' name reflecting the group as a character. Default is NULL
#' @param fraction Percentage of the dataset that should be subset as the
#' training set. Default is 0.6 to indicate that 60% of the dataset will be
#' randomly sampled to comprise the training set.
#' @param validation Default is FALSE. If there is a reason to have a
#' validation set in addition to a test set, specify this as TRUE. The
#' remaining data left from partitioning the training set are then randomly
#' and equally split into the validation and test sets. 
#' 
#' @returns A list of 2 or 3 dataframes representing the training, validation,
#' and test sets.
#' 
#' @export
test_split <- function(data = .,
                       group = NULL,
                       proportion = .6,
                       validation = FALSE){
  
  # If no groups exist, then split on total data
  if(is.null(group)){
    
    if(isTRUE(validation)){
      
      # Get training data
      train <- data %>% 
        sample_frac(proportion)
      
      # Get validation data
      validation <- data %>% 
        anti_join(train) %>% 
        sample_frac(.5)
      
      # Get test data
      test <- data %>% 
        anti_join(train) %>% 
        anti_join(validation)
      
    } else {
      
      # Get training data
      train = data %>%
        sample_frac(proportion)
      
      # Get test data
      test = data %>% 
        anti_join(train)
      
    }
    
    # If separate groups exist, then split by group:
  } else {
    
    if(isTRUE(validation)){
      
      # Get training data
      train = data %>%
        group_by(!!sym(group)) %>% 
        sample_frac(proportion)
      
      # Get validation data
      validation = data %>% 
        group_by(!!sym(group)) %>% 
        anti_join(train) %>% 
        sample_frac(.5)
      
      # Get test data
      test = data %>% 
        group_by(!!sym(group)) %>% 
        anti_join(train) %>% 
        anti_join(validation)
      
    } else {
      
      # Get training data
      train = data %>%
        group_by(!!sym(group)) %>% 
        sample_frac(proportion)
      
      # Get test data
      test = data %>% 
        group_by(!!sym(group)) %>% 
        anti_join(train)
      
    }
    
  }
  
  # If validation is TRUE
  if(isTRUE(validation)){
    
    ml_list = list(train = train,
                   valid = validation,
                   test = test)
    
  } else {
    
    ml_list = list(train = train,
                   test = test)
    
  }
  
  # Return results as list
  return(ml_list)
  
}