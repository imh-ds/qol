#' Descriptive Statistics Wrapper
#'
#' @description A simple function for automating the calculation of general
#'   descriptive statistics, like means, skew, missingness, reliability, and
#'   correlation matrices. The 'engine' of the correlation matrix is derived
#'   from Stefan Engineering's code (Stefan Engineering, 2018).
#'
#' @param data A dataframe object. This should be a structured dataset where
#'   each column represents a variable and each row represents an observation.
#' @param varlist An optional named list of vectors. Each name in the list
#'   represents a composite or latent variable, and the corresponding vector
#'   contains the column names that are associated with the indicators
#'   comprising said composite or latent variable. Default is NULL.
#' @param digits Number of decimal places for the correlation matrix. Default is
#'   3 decimal points. If not specified, the function will use the default
#'   value.
#'
#' @returns A list of dataframes containing the general descriptives of all
#'   variables in the dataset from \code{psych::describe()} and variable
#'   missingness information (Revelle, 2023). If the varlist was specified of
#'   latent or composite variables and their indicators, then three additional
#'   dataframes are returned. These include a new dataset with the calculated
#'   latent or composite variables using the specified indicators, a correlation
#'   matrix of the latent or composite variables, and a measurement reliability
#'   table of the latent or composite variables. The latent or composite
#'   variables are calculated as the unweighted means of all indicators. The
#'   measurement reliability table returns the Cronbach's alpha and McDonald's
#'   omega as calculated by the jamovi package's \code{jmv::reliability()}
#'   function (Selker et al., 2022).
#'
#' @examples
#' varlist <- list(anxiety = c("anx_1", "anx_2", "anx_3"),
#'                 sleep = c("slp_1", "slp_2"),
#'                 life_sat = c("lsat_1", "lsat_2", "lsat_3"))
#' desc_wrapper(data, varlist, digits = 3)
#'
#' @references William Revelle (2023). psych: Procedures for Psychological,
#'   Psychometric, and Personality Research. Northwestern University, Evanston,
#'   Illinois. R package version 2.3.3,
#'   \url{https://CRAN.R-project.org/package=psych}.
#'
#'   Selker R, Love J, Dropmann D, Moreno V (2022). jmv: The 'jamovi' Analyses.
#'   R package version 2.3.4, \url{https://CRAN.R-project.org/package=jmv}.
#'
#'   Stefan Engineering (2018). Create an APA style correlation table with R.
#'   \url{https://stefaneng.github.io/apa_correlation_table/}.
#'
#' @export
desc_wrapper <- function(
    data = .,
    composite_list = NULL,
    weight = "correlation",
    decay_rate = 0.5,
    sigma = 0.5,
    return_metrics = TRUE,
    digits = 3,
    type = "pearson",
    p_thresholds = c(0.05, 0.01, 0.001),
    p_stars = TRUE,
    msd_position = "left"
){
  
  # If composite list is given, then get reliability metrics for each composite
  
  if(!is.null(composite_list)){
    
    # Composite score
    cs <- cscore::composite_score(
      
      data = data,
      composite_list = composite_list,
      weight = weight,
      decay_rate = decay_rate,
      sigma = sigma,
      return_metrics = return_metrics,
      digits = digits
      
    )
    
    # Get data
    cs_df <- cs[["data"]]
    
    # Get loadings & weights
    cs_lw <- cs[["metrics"]]
    
    # Get reliability & validity
    cs_rl <- cs[["validity"]]
    
    # Composite order
    cs_od <- composite_list[["order"]]
    
    # Get correlation matrix
    cor_mat <- cor_matrix(
      
      data = cs_df,
      variables = cs_od,
      digits = digits,
      type = type,
      p_thresholds = p_thresholds,
      p_stars = p_stars,
      msd_position = msd_position
      
    )
    
  }
  

  # Descriptive for data
  data_desc <- data.frame(psych::describe(data)) %>% 
    tibble::rownames_to_column(var = "variable")
  
  # Data missingness
  data_missing <- data %>%
    dplyr::summarise_all(list(~sum(is.na(.))/nrow(data))) %>%
    tidyr::gather(key = "variable",
                  value = "missingness") %>% 
    dplyr::mutate(n_total = nrow(data),
                  n_missing = n_total*missingness)
  
  
  # Report results
  if(!is.null(composite_list)){
    
    sheets <- list(
      "desc" = data_desc,
      "miss" = data_missing,
      "load" = cs_lw,
      "reli" = cs_rl,
      "corr" = cor_mat
    )
    
  } else {
    
    sheets <- list(
      "desc" = data_desc,
      "miss" = data_missing
    )
    
  }
  
  # Return
  return(sheets)
  
}
