
#' Augmented Data Accuracy Evaluation
#'
#' @description
#' Calculate the fit metrics of the augmented data against the observed data.
#'
#' @param aug_vals 
#' @param org_vals 
#'
aug_fit <- function(
    
  aug_vals,
  org_vals
  
) {
  
  # Calculate the differences against original data
  differences <- abs(aug_vals - org_vals)
  raw_differences <- aug_vals - org_vals
  
  # Calculate accuracy of augmented against original data
  accuracies <- 1 - (differences / abs(org_vals))
  
  # Convert the differences to a vector and calculate the mean
  output <- data.frame(
    avg_accuracy = mean(as.vector(accuracies), na.rm = TRUE),
    sd_accuracy = sd(as.vector(accuracies), na.rm = TRUE),
    differences = mean(as.vector(differences), na.rm = TRUE),
    sd_differences = sd(as.vector(differences), na.rm = TRUE),
    distance = mean(as.vector(abs(aug_vals)), na.rm = TRUE),
    sd_distance = sd(as.vector(abs(aug_vals)), na.rm = TRUE),
    raw_differences = mean(as.vector(raw_differences), na.rm = TRUE),
    sd_raw_differences = sd(as.vector(raw_differences), na.rm = TRUE)
  )
  
  # Return
  return(output)
  
}