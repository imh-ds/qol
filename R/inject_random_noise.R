
#' Inject Random Noise
#'
#' @param data 
#' @param iter 
#' @param vars 
#'
#' @return
#' @export
#'
#' @examples
inject_noise <- function(data = .,
                         iter = 1,
                         vars = NULL) {
  
  
  if (is.null(vars)) {
    
    names <- names(data)
    
  } else {
    
    names <- vars
    
  }
  
  
  for (n in iter) {
    
    # Loop over each column in the dataframe
    for (col_name in names) {
      
      # Check if the column is numeric
      if (is.numeric(data[[col_name]])) {
        
        # Loop over each value in the column
        for (i in 1:length(data[[col_name]])) {
          
          # Get the range of values in the column
          min_val <- min(data[[col_name]], na.rm = TRUE)
          max_val <- max(data[[col_name]], na.rm = TRUE)
          
          # If the value is NA, skip to the next iteration
          if (is.na(data[i, col_name])) {
            next
          }
          
          # If the value is the minimum, randomly add 0 or 1
          else if (data[i, col_name] == min_val) {
            data[i, col_name] <- data[i, col_name] + sample(0:1, 1)
          }
          
          # If the value is the maximum, randomly add -1 or 0
          else if (data[i, col_name] == max_val) {
            data[i, col_name] <- data[i, col_name] + sample(-1:0, 1)
          }
          
          # Otherwise, randomly add -1, 0, or 1
          else {
            data[i, col_name] <- data[i, col_name] + sample(-1:1, 1)
          }
          
        }
      }
    }
    
  }
  
  
  # Return the modified dataframe
  return(data)
  
}