#' Linear Discriminant Analysis Function Coefficients
#'
#' @description Runs linear discriminant analysis (LDA) through the \code{MASS}
#'   package's \code{lda()} function. As the \code{lda()} function does not
#'   offer canonical function coefficients by default, this function calculates
#'   this to mirror SPSS's output of coefficients through their discriminant
#'   function analysis (DFA) approach.
#'
#' @param data A dataframe object. This should be a structured dataset where
#'   each column represents a variable and each row represents an observation.
#' @param outcome A required string of a column name representing the outcome
#'   variable.
#' @param inputs A required vector of column names representing the input
#'   variables.
#'
#' @return The standard \code{lda} model object with additional appended
#'   coefficients and predicted classes within the list.
#'
#' @examples
#' ldfa(iris,
#'      outcome = "species",
#'      inputs = c("Sepal.Length", "Sepal.Width",
#'                 "Petal.Length", "Petal.Width"))
#'
#' @references Venables, W. N. & Ripley, B. D. (2002) Modern Applied Statistics
#'   with S. Fourth Edition. Springer, New York. ISBN 0-387-95457-0
#'
#' @export
ldfa <- function(data,
                 outcome,
                 inputs) {
  
  # Get input data
  input_data <- data %>% 
    dplyr::select(dplyr::all_of(inputs))
  
  # Get groups
  groups <- data[[outcome]]
  
  # Create formula
  formula <- as.formula(paste0(outcome,
                               " ~ ",
                               paste0(inputs,
                                      collapse = " + ")))
  
  
  # Perform linear discriminant analysis
  lda_result <- MASS::lda(formula,
                          data = data)
  
  # Get the number of unique groups
  num_groups <- length(unique(groups))
  
  # Get the number of columns in input_data
  num_vars <- ncol(input_data)
  
  # Get the means of each group
  group_means <- lda_result$means
  
  # Initialize an array to hold the within-group covariance matrices
  within_group_covariance <- array(NA,
                                   dim = c(num_vars,
                                           num_vars,
                                           num_groups))
  
  
  # Loop over each group
  for (i in 1:num_groups) {
    
    
    # Scale the data for the current group (centering it by subtracting the mean)
    scaled_data <- scale(subset(input_data,
                                groups == unique(groups)[i]),
                         scale = FALSE)
    
    # Compute the within-group covariance matrix for the current group
    within_group_covariance[, ,i] <- t(scaled_data) %*% scaled_data
    
    
  }
  
  
  # Initialize the pooled within-group covariance matrix
  pooled_covariance <- within_group_covariance[, ,1]
  
  # Sum the within-group covariance matrices to get the pooled within-group covariance matrix
  for (i in 2:num_groups) pooled_covariance <- pooled_covariance + within_group_covariance[, ,i]
  
  # Compute the average within-group covariance matrix
  avg_covariance <- pooled_covariance / (nrow(input_data) - num_groups)
  
  # Compute the inverse of the average within-group covariance matrix
  inv_avg_covariance <- solve(avg_covariance)
  
  # Initialize a matrix to hold the classification function coefficients
  class_functions <- matrix(NA,
                            nrow = num_vars + 1,
                            ncol = num_groups)
  
  # Set the column names of the classification function coefficients matrix
  colnames(class_functions) <- lda_result[["lev"]]
  
  # Set the row names of the classification function coefficients matrix
  rownames(class_functions) <- c("Constant",
                                 colnames(input_data))
  
  
  # Loop over each group
  for (i in 1:num_groups) {
    
    
    # Compute the constant term for the classification function for the current group
    class_functions[1, i] <- -0.5 * t(group_means[i,]) %*% inv_avg_covariance %*% (group_means[i,])
    
    # Compute the coefficients for the classification function for the current group
    class_functions[2:(num_vars+1),i] <- inv_avg_covariance %*% (group_means[i,])
    
    
  }
  
  
  # Add the classification function coefficients to the lda object
  lda_result[["class_functions"]] <- class_functions
  
  # Get predicted memberships
  predictions <- stats::predict(lda_result)
  
  # Store predicted classes
  lda_result[["predictions"]] <- predictions[["class"]]
  
  # Return the lda object with the added classification function coefficients
  return(lda_result)
  
  
}
