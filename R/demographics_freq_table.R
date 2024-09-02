#' DEMOGRAPHICS FREQUENCY TABLE
#'
#' @description Create a demographic frequency table that provides the count and
#' proportion of each variable value. The current version does not support
#' automatically extracting variable labels from SPSS files through \code{haven}
#' (Wickham et al., 2023). The variable values will be returned as is, meaning
#' that if the variables are numeric, then the table will display numeric
#' values. Users are recommended to recode the demographic variables of interest
#' with string values as necessary.
#'
#' @param data A dataframe object. This should be a structured dataset where
#'   each column represents a variable and each row represents an observation.
#' @param variables A required vector of column names representing their
#'   manifest demographic variables.
#'
#' @returns A dataframe of the frequencies of each category
#'
#' @examples
#' variables <- c("age", "education", "gender")
#' demographic_table(data, variables)
#'
#' @references Wickham H, Miller E, Smith D (2023). _haven: Import and Export
#'   'SPSS', 'Stata' and 'SAS' Files_. R package version 2.5.2,
#'   \url{https://CRAN.R-project.org/package=haven}.
#'
#' @export
demographic_table <- function(
    data = .,
    variables
){
  
  # Create empty list
  dem_list <- list()
  
  # Run loop for every variable
  for(var in variables){
    
    # Run frequency table
    frequency <- data %>% 
      freq(., var)
    
    # Get total n value
    total <- frequency %>% 
      dplyr::ungroup() %>% 
      dplyr::summarize(
        total = sum(n, na.rm = T)
      ) %>% 
      .$total
    
    # Mutate proportion of total and reorganize frequency table
    frequency <- frequency %>% 
      dplyr::mutate(
        percent = paste0(
          sprintf('%.2f',
                  100*(n / total)),
          "%"
        ),
        Variable = var
      ) %>% 
      dplyr::select(
        Variable, 
        dplyr::everything()
      ) %>% 
      magrittr::set_colnames(
        ., 
        c("Variable",
          "Value",
          "n",
          "Proportion")
      )
    
    # Save to demographic list
    dem_list[[var]] <- frequency
    
  }
  
  # Reduce to working table
  dem_table <- purrr::reduce(
    dem_list,
    rbind
  )
  
  # Return working table
  return(dem_table)
  
}