#' Inject Random Missingness
#'
#' @param data 
#' @param prop 
#' @param vars description
#' @param method description
#'
#' @export
inject_na <- function(
    data = .,
    prop = .10,
    vars = NULL,
    method
) {
  
  # If no variables are specified, then get all variables in dataframe
  names <- if (is.null(vars)) {
    
    names(data)
    
  } else {
    
    vars
    
  }
  

  # METHOD - BY ROW ---------------------------------------------------------

  if (method == "row") {
    
    # For each row in the dataframe
    na_df <- lapply(
      seq(nrow(data)),
      function(i) {
        
        # Sample one row with replacement
        row <- data[i,]
        
        # Randomly select n% of the columns to be NA
        row[sample(names,
                   size = round(prop * ncol(data)))] <- NA
        
        # Return
        return(row)
        
      }
      
    )
    
    # Reduce
    na_df <- purrr::reduce(
      na_df,
      rbind
    )
    
  }
  
  

  # METHOD - BY COLUMN ------------------------------------------------------

  if (method == "column") {
    
    na_df <- data
    
    for (i in names) {
      
      # Number of rows to set as NA
      num_na <- round(prop * nrow(na_df))
      
      # Randomly select rows to set as NA
      na_rows <- sample(
        seq_len(nrow(na_df)),
        size = num_na
      )
      
      # Set the selected rows as NA for the current column
      na_df[na_rows, i] <- NA
      
    }
    
  }
  
  

  # RETURN ------------------------------------------------------------------

  # Return
  return(na_df)
  
}
