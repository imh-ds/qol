#' DESCRIPTIVE STATISTICS WRAPPER
#' 
#' @description
#' A simple function for automating the calculation of general descriptive
#' statistics, like means, skew, missingness, reliability, and correlation
#' matrices. The 'engine' of the correlation matrix is derived from Stefan
#' Engineering's code (Stefan Engineering, 2018).
#' 
#' @param data A dataframe object. This should be a structured dataset where
#' each column represents a variable and each row represents an observation.
#' @param varlist An optional named list of vectors. Each name in the list
#' represents a composite or latent variable, and the corresponding vector
#' contains the column names that are associated with the indicators comprising
#' said composite or latent variable. Default is NULL.
#' @param digits Number of decimal places for the correlation matrix. Default
#' is 3 decimal points. If not specified, the function will use the default
#' value.
#' 
#' @returns A list of dataframes containing the general descriptives of all
#' variables in the dataset from \code{psych::describe()} and variable missingness
#' information (Revelle, 2023). If the varlist was specified of latent or
#' composite variables and their indicators, then three additional dataframes
#' are returned. These include a new dataset with the calculated latent or
#' composite variables using the specified indicators, a correlation matrix of
#' the latent or composite variables, and a measurement reliability table of the
#' latent or composite variables. The latent or composite variables are 
#' calculated as the unweighted means of all indicators. The measurement
#' reliability table returns the Cronbach's alpha and McDonald's omega as
#' calculated by the jamovi package's \code{jmv::reliability()} function (Selker et
#' al., 2022).
#' 
#' @examples
#' varlist <- list(anxiety = c("anx_1", "anx_2", "anx_3"),
#'                 sleep = c("slp_1", "slp_2"),
#'                 life_sat = c("lsat_1", "lsat_2", "lsat_3"))
#' desc_wrapper(data, varlist, digits = 3)
#' 
#' @references William Revelle (2023). psych: Procedures for Psychological,
#' Psychometric, and Personality Research. Northwestern University, Evanston,
#' Illinois. R package version 2.3.3,
#' \url{https://CRAN.R-project.org/package=psych}.
#' 
#' Selker R, Love J, Dropmann D, Moreno V (2022). jmv: The 'jamovi' Analyses.
#' R package version 2.3.4, \url{https://CRAN.R-project.org/package=jmv}.
#' 
#' Stefan Engineering (2018). Create an APA style correlation
#' table with R. \url{https://stefaneng.github.io/apa_correlation_table/}.
#' 
#' @export
desc_wrapper = function(data = .,
                        varlist = NULL,
                        digits = 3){
  
  # If varlist is given, then get reliability metrics for each composite
  
  # Reliability
  reliability_list = list()
  if(!is.null(varlist)){
    
    for(x in seq_along(names(varlist))){
      
      reliability_list[[x]] = jmv::reliability(data = data,
                                               vars = varlist[[x]],
                                               omegaScale = T,
                                               meanScale = T,
                                               sdScale = T)$scale$asDF
      
    }
    
    reli_table = reliability_list %>%
      purrr::reduce(rbind)
    
    reli_table = reli_table %>% 
      rownames_to_column(var = "Variable") %>% 
      mutate(Variable = names(varlist))
    
  }
  
  # If varlist is given, then get correlation matrix
  if(!is.null(varlist)){
    
    for(x in seq_along(names(varlist))){
      
      varname <- names(varlist)[x]
      
      data <- data %>% 
        mutate(!!varname := rowMeans(x = select(., varlist[[x]]), na.rm = T))
      
    }
    
    cormat <- cor_matrix(data = data,
                         variables = names(varlist),
                         digits = digits)
    
  }
  
  
  # Descriptive for data
  datadesc = data %>%
    psych::describe() %>% as.data.frame() %>%
    rownames_to_column(var = "variable")
  
  # Data missingness
  datamissing = data %>%
    summarise_all(list(~sum(is.na(.))/nrow(data))) %>%
    gather(key = "variable",
           value = "missingness") %>% 
    mutate(n_total = nrow(data),
           n_missing = n_total*missingness)
  
  
  # Report results
  if(!is.null(varlist)){
    
    sheets = list("data" = data,
                  "desc" = datadesc,
                  "miss" = datamissing,
                  "reli" = reli_table,
                  "corr" = cormat)
    
  } else {
    
    sheets = list("desc" = datadesc,
                  "miss" = datamissing)
    
  }
  
  return(sheets)
  
}