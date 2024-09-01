#' Calculate Means by Group
#'
#' @description Calculate variable means by group.
#'
#' @param data A dataframe object. A structured dataset where each column
#'   represents a variable and each row represents an observation.
#' @param vars A required vector of variable names.
#' @param group A string character indicating the name of the group variable.
#' @param as_matrix A logical indicating whether to return a matrix or data
#'   frame. Options include \code{FALSE} to return a class \code{data.frame} and
#'   \code{TRUE} to return a class \code{matrix}.
#'
#' @return
#'
#' @export
means_by <- function(
  data = .,
  vars,
  group,
  as_matrix = FALSE
) {
  
  # Clean data
  data <- data %>% 
    dplyr::select(
      dplyr::all_of(
        c(vars,
          group)
      )
    )
  
  # Get means matrix
  matrix <- data %>% 
    
    # Group by
    dplyr::group_by(
      !!rlang::sym(group)
    ) %>% 
    
    # Calculate means by group
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(vars),
        ~ mean(.x, na.rm = TRUE),
        .names = "{col}"
      )
    ) %>% 
    
    # Pivot
    tidyr::pivot_longer(
      cols = -!!rlang::sym(group),
      names_to = "Variable",
      values_to = "value"
    ) %>% 
    tidyr::pivot_wider(
      names_from = !!rlang::sym(group),
      values_from = value
    ) %>% 
    
    # As data frame
    as.data.frame()
  
  # If specified to return a matrix
  if (isTRUE(as_matrix)) {
    
    matrix <- as.matrix(matrix[-1])
    
    rownames(matrix) <- vars
    
  }
  
  # Return
  return(matrix)
  
}


#' Calculate a Double-Center Matrix
#'
#' @description Calculate a double-centered matrix.
#'
#' @param data A dataframe object. A structured dataset where each column
#'   represents a variable and each row represents an observation.
#' @param vars A required vector of variable names.
#' @param group A string character indicating the name of the group variable.
#' @param as_matrix A logical indicating whether to return a matrix or data
#'   frame. Options include \code{FALSE} to return a class \code{data.frame} and
#'   \code{TRUE} to return a class \code{matrix}.
#'
#' @return
#'
#' @export
double_center <- function(
    data = .,
    vars,
    group,
    as_matrix = FALSE
) {
  
  # Calculate matrix
  matrix <- means_by(
    data = data,
    vars = vars,
    group = group,
    as_matrix = TRUE
  )
  
  # Get dimensions of matrix
  row_n <- nrow(matrix)
  col_n <- ncol(matrix)
  
  # Get row means of matrix
  row_matrix <- matrix(
    data = rowMeans(matrix),
    nrow = row_n,
    ncol = col_n
  )
  
  # Get column means of matrix
  col_matrix <- t(
    matrix(
      data = colMeans(matrix),
      nrow = row_n,
      ncol = col_n
    )
  )
  
  # Calculate double-centered matrix
  dc_matrix <- matrix - row_matrix - col_matrix + mean(matrix)
  
  # If the double-centered matrix should be returned as a dataframe instead
  if (isFALSE(as_matrix)) {
    
    dc_matrix <- as.data.frame(dc_matrix) %>% 
      tibble::rownames_to_column(
        var = "Variable"
      )
    
  }
  
  # Return
  return(dc_matrix)
  
}
