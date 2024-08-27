
#' Inject Random Noise
#'
#' @param data 
#' @param iter 
#' @param vars 
#'
#' @export
inject_noise <- function(
    data = .,
    iter = 1,
    vars = NULL,
    noise_value = 1,
    digits = NA
) {
  
  # If no variables are specified, then get all variables in dataframe
  names <- if (is.null(vars)) {
    
    names(data)
    
  } else {
    
    vars
    
  }
  
  
  # Iterate noise injection n times
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
          
          # # If the value is the minimum, randomly add 0 or 1
          # else if (data[i, col_name] == min_val) {
          #   data[i, col_name] <- data[i, col_name] + sample(0:noise_value, 1)
          # }
          # 
          # # If the value is the maximum, randomly add -1 or 0
          # else if (data[i, col_name] == max_val) {
          #   data[i, col_name] <- data[i, col_name] + sample(-noise_value:0, 1)
          # }
          # 
          # # Otherwise, randomly add -1, 0, or 1
          # else {
          #   data[i, col_name] <- data[i, col_name] + sample(-noise_value:noise_value, 1)
          # }
          
          # Randomly add noise
          else {
            
            data[i, col_name] <- data[i, col_name] + runif(1,
                                                           min = -noise_value,
                                                           max = noise_value)
            
            # Cap ceiling
            if (data[i, col_name] > max_val) {
              data[i, col_name] <- max_val
            }
            
            # Cap floor
            if (data[i, col_name] < min_val) {
              data[i, col_name] <- min_val
            }
            
            # If rounding
            if (!is.na(digits)) {
              
              data[i, col_name] <- round(data[i, col_name],
                                         digits = digits)
              
            }
            
          }

          
        }
      }
    }
    
  }
  
  
  # Return the modified dataframe
  return(data)
  
}