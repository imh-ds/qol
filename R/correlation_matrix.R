#' Create a correlation matrix
#'
#' @description Create a correlation matrix with variable mean and standard
#'   deviation descriptive statistics. The 'engine' of the correlation matrix is
#'   inspired from Stefan Engineering's code (Stefan Engineering, 2018).
#'
#' @param data A dataframe object. A structured dataset where each column
#'   represents a variable and each row represents an observation.
#' @param variables A required vector of column names representing their
#'   manifest variables. For latent and composite variables that are comprised
#'   of multiple indicators, use the \code{desc_wrapper()} function and specify
#'   a named list of variables in the \code{varlist} argument.
#' @param digits Number of decimal places for the correlation matrix. Default is
#'   3 decimal points. If not specified, the function will use the default
#'   value.
#'
#' @return A correlation matrix dataframe with variable mean and standard
#'   deviation. The dataframe can be exported as an excel workbook to be copy
#'   and pasted into a word document or LaTeX.
#'
#' @examples
#' variables <- c("age", "education", "income")
#' cor_matrix(data = data, variables = variables, digits = 3)
#'
#' @references Stefan Engineering (2018). Create an APA style correlation table
#'   with R. \url{https://stefaneng.github.io/apa_correlation_table/}.
#'
#' @export
cor_matrix <- function(data = .,
                       variables,
                       digits = 3,
                       type = "pearson",
                       p_thresholds = c(0.05, 0.01, 0.001),
                       p_stars = TRUE,
                       msd_position = "left"){
  
  # Get relevant variables
  df <- data %>% 
    dplyr::select(dplyr::all_of(variables))
  
  # Create rcorr matrix
  mat <- df %>% 
    # Convert to matrix
    base::as.matrix() %>% 
    # Create correlation matrix
    Hmisc::rcorr(., type = type)
  
  # Round the matrix
  mat_rounded <- round(mat$r,
                       digits)
  
  # Format the matrix to designated decimal points
  mat_formatted <- format(mat_rounded,
                          nsmall = digits)
  
  # If p-value stars are set to TRUE
  if(isTRUE(p_stars)){
    
    # Add stars for p-value thresholds
    cmat <- ifelse(mat$P < p_thresholds[1],
                   paste0(mat_formatted, "*"),
                   mat_formatted)
    cmat <- ifelse(mat$P < p_thresholds[2],
                   paste0(cmat, "*"),
                   cmat)
    cmat <- ifelse(mat$P < p_thresholds[3],
                   paste0(cmat, "*"),
                   cmat)
    
  } else {
    
    cmat <- mat_formatted
    
  }

  
  # Put - on diagonal and blank on upper diagonal
  cmat[upper.tri(cmat, diag = TRUE)]  <- "-"
  cmat[upper.tri(cmat, diag = FALSE)] <- ""
  
  # Remove _ and convert to title case
  rownames(cmat) <- tools::toTitleCase(gsub("_",
                                             " ",
                                             rownames(cmat)))
  
  # Add index number to row names
  rownames(cmat) <- paste0(seq_along(rownames(cmat)),
                            ".",
                            rownames(cmat))
  
  # Convert to data frame
  cortable <- as.data.frame(cmat) %>% 
    # Create numbered column names
    magrittr::set_colnames(., seq(ncol(.)))
  
  
  
  # Get means of variables
  mean <- df %>% 
    dplyr::summarise(dplyr::across(.cols = tidyr::everything(),
                                   \(x) mean(x, na.rm = TRUE)))
  
  # Get standard deviations of variables
  stdev <- df %>% 
    dplyr::summarise(dplyr::across(.cols = tidyr::everything(),
                                   \(x) sd(x, na.rm = TRUE)))
  
  # IF M & SD position is set to LEFT
  if(msd_position == "left"){
    
    # Transpose M
    mean <- mean %>% 
      t() %>% 
      magrittr::set_colnames(., "M")
    
    # Transpose SD
    stdev <- stdev %>% 
      t() %>% 
      magrittr::set_colnames(., "SD")
    
    # Create correlation matrix
    cormatrix <- cortable %>% 
      # Column bind the mean and stdev
      cbind(mean,
            stdev) %>%
      # Create Variable column from row names
      tibble::rownames_to_column(var = "Variable") %>% 
      # Reorder
      dplyr::select(Variable,
                    M,
                    SD,
                    tidyr::everything())
    
  }
  
  # IF M & SD position is set to bottom
  if(msd_position == "bottom"){
    
    desc <- rbind(mean,
                  stdev) %>% 
      dplyr::mutate(Variable = c("M", "SD")) %>% 
      dplyr::select(Variable, tidyr::everything()) %>% 
      magrittr::set_colnames(., c("Variable",
                                  seq(ncol(.)-1)))
    
    # Create correlation matrix
    cormatrix <- cortable %>% 
      # Create Variable column from row names
      tibble::rownames_to_column(var = "Variable") %>% 
      # Reorder
      dplyr::select(Variable,
                    tidyr::everything()) %>% 
      # Row bind the mean and stdev
      rbind(desc)
    
  }
  
  
  # Return the correlation matrix
  return(cormatrix)
  
  
}
