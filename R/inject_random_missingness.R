
#' Inject Random Missingness
#'
#' @param data 
#' @param prop 
#'
#' @export
inject_na <- function(data = .,
                      prop = .10) {
  
  # For each row in the dataframe
  na_df <- lapply(
    
    seq(nrow(data)),
    function(i) {
      
      # Sample one row with replacement
      row <- data[i,]
      
      # Randomly select 70% of the columns to be NA
      row[sample(ncol(data),
                 size = round(prop * ncol(data)))] <- NA
      
      # Return
      return(row)
      
    }
    
  )
  
  # Reduce
  na_df <- purrr::reduce(na_df,
                         rbind)
  
  # Return
  return(na_df)
  
}