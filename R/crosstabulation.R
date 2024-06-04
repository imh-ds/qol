#' Crosstabulation
#'
#' @description This function creates a cross-tabulation of two variables in a
#'   data frame, adds a total column and row, and replaces NA values with
#'   "<NA>".
#'
#' @param data A data frame object. This should be a structured dataset where
#'   each column represents a variable and each row represents an observation.
#' @param x A string of the first variable column name.
#' @param y A string of the second variable column name.
#'
#' @returns A data frame with the cross-tabulation.
#'
#' @examples
#' \dontrun{
#' crosstab(data = df, x = "var1", y = "var2")
#' }
#'
#' @export
crosstab <- function(data = .,
                     x,
                     y){
  
  # Group by the two variables, tally the counts, spread into a wide format,
  # and rename the first column
  ct <- data %>% 
    dplyr::group_by(!!rlang::sym(x),
                    !!rlang::sym(y)) %>% 
    dplyr::tally() %>% 
    tidyr::spread(!!rlang::sym(y), n) %>% 
    as.data.frame() %>% 
    dplyr::rename(!!rlang::sym(y) := !!rlang::sym(x))
  
  # Create a new column with the first value being `x` and the rest being `NA`
  new_col <- c(x,
               rep(NA,
                   nrow(ct)-1))
  
  # Add the new column at the beginning of the data frame
  ct <- cbind(Variables = new_col, ct)
  
  # Add a new column that is the sum of all columns from the 3rd to the end
  ct <- ct %>% 
    dplyr::mutate(Total = rowSums(.[,3:ncol(.)], na.rm = TRUE))
  
  # Create a new row that is the sum of all rows by column
  total_row <- data.frame(matrix(ncol = ncol(ct),
                                 nrow = 1))
  colnames(total_row) <- colnames(ct)
  total_row[1,  2] <- "Total"
  total_row[1, -(1:2)] <- colSums(ct[,-(1:2)],
                                  na.rm = TRUE)
  ct <- rbind(ct,
              total_row)
  
  # Replace NA values in the second column with "<NA>"
  ct[[y]][is.na(ct[[y]])] <- "<NA>"
  
  # Return crosstab
  return(ct)
  
}
