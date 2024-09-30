#' Multiple Confirmatory Factor Analysis Wrapper
#'
#' @description A wrapper function to automate the running of multiple
#'   confirmatory factor analyses (CFA).
#'
#' @param data A data frame. Each column represents a variable and each row an
#'   observation.
#' @param models A list of named CFA models compatible with \code{lavaan}. See
#'   \code{lavaan} documentation for instructions on specifying a CFA model.
#' @param cluster An optional character argument. A variable name defining
#'   clusters in a multi-level dataset.
#' @param missing Method for handling missing data. Default is "listwise", which
#'   deletes all observations with missing values prior to analysis. This is
#'   recommended when data are missing completely at random (MCAR). Another
#'   option is "ml" for full information maximum likelihood (FIML). See
#'   \code{lavaan} documentation for more details.
#' @param se Method for computing standard errors. Default is NULL. Set to
#'   "robust" to use either "robust.sem" or "robust.huber.white" depending on
#'   the estimator used. See \code{lavaan} documentation for more details.
#' @param bootstrap An integer specifying the number of bootstrap iterations. A
#'   higher value provides more robust results but increases computational time.
#'   Default is 1000.
#' @param estimator Estimator for the CFA. Default is maximum likelihood (ML).
#'   Other estimators are those offered by \code{lavaan}, e.g., "GLS", "WLS",
#'   "DWLS", "ULS", "DLS", and "PML". See \code{lavaan} documentation for more
#'   details.
#' @param digits An integer specifying the number of decimal places for rounding
#'   in the in-text reference generator.
#' @param standardized A logical value indicating whether to return standardized
#'   or unstandardized estimates. If \code{TRUE}, the function returns
#'   standardized estimates. If \code{FALSE}, it returns unstandardized
#'   estimates. Default is \code{TRUE}.
#' @param mi_groups A vector of column names representing the categorical group
#'   variables for testing measurement invariance. Default is NULL which means
#'   measurement invariance is not run. If specified, the function will run
#'   measurement invariance on the groups and return fit metrics.
#'
#' @return A list containing \code{lavaan} CFA models and data frames with the
#'   results of the analysis. The basic output includes data frame tables for
#'   goodness of fit metrics and model estimations. If measurement invariance
#'   groups were specified, fit indices and goodness of fit comparison tables
#'   are also included in the output.
#'
#' @examples
#' # Note: Model example courtesy of 'lavaan'
#'
#' model1 <- '
#'   # measurement model
#'     ind60 =~ x1 + x2 + x3
#'     dem60 =~ y1 + y2 + y3 + y4
#'     dem65 =~ y5 + y6 + y7 + y8
#'  '
#'
#' model2 <- '
#'   # measurement model
#'     ind60 =~ x1 + x2
#'     dem60 =~ y1 + y2
#'     dem65 =~ y5 + y6
#'  '
#'
#' model_list <- list(First_Model = model1,
#'                    Second_Model = model2)
#'
#' cfa_table(data = data,
#'           models = model_list)
#'
#'
#' @references Yves Rosseel (2012). lavaan: An R Package for Structural Equation
#' Modeling. \emph{Journal of Statistical Software}, 48(2), 1-36.
#' https://doi.org/10.18637/jss.v048.i02
#'
#' @export
wrap_multi_cfa <- function(
    
  data = .,
  models,
  cluster = NULL,
  missing = "listwise",
  se = NULL,
  bootstrap = 1000,
  estimator = "ML",
  digits = 3,
  standardized = TRUE,
  mi_groups = NULL
  
){
  
  # Repeat for every model
  results <- lapply(seq_along(models), 
                    function(m) {
    
    # Grab Model
    mod <- models[[m]]
    
    # Get Model Name
    mod_name <- names(models)[[m]]
    
    # Run CFA Wrapper by model
    cfa_result <- wrap_cfa(
      
      data = data,
      model = mod,
      name = mod_name,
      cluster = cluster,
      missing = missing,
      se = se,
      bootstrap = bootstrap,
      estimator = estimator,
      digits = digits,
      standardized = standardized,
      mi_groups = mi_groups
      
    )
    
    # Add Model names to MI results
    cfa_result[["mi_nested"]] <- cfa_result[["mi_nested"]] %>% 
      dplyr::mutate(model = c(mod_name,
                              rep(NA, nrow(.)-1))) %>% 
      dplyr::select(model, dplyr::everything())
    
    cfa_result[["mi_fit"]] <- cfa_result[["mi_fit"]] %>% 
      dplyr::mutate(model = c(mod_name,
                              rep(NA, nrow(.)-1))) %>% 
      dplyr::select(model, dplyr::everything())
    
    cfa_result[["mi_fit_total"]] <- cfa_result[["mi_fit_total"]] %>% 
      dplyr::mutate(model = c(mod_name,
                              rep(NA, nrow(.)-1))) %>% 
      dplyr::select(model, dplyr::everything())
    
    cfa_result[["mi_fitdif"]] <- cfa_result[["mi_fitdif"]] %>% 
      dplyr::mutate(model = c(mod_name,
                              rep(NA, nrow(.)-1))) %>% 
      dplyr::select(model, dplyr::everything())
    
    cfa_result[["mi_fitdif_total"]] <- cfa_result[["mi_fitdif_total"]] %>% 
      dplyr::mutate(model = c(mod_name,
                              rep(NA, nrow(.)-1))) %>% 
      dplyr::select(model, dplyr::everything())
    
    
    # Return
    return(cfa_result)
    
  })
  
  # Extract the results
  cfa_models        <- lapply(results, function(x) x[["model"]])
  cfa_fit_table     <- lapply(results, function(x) x[["fit_table"]])
  cfa_fit_list      <- lapply(results, function(x) x[["fit_table_full"]])
  cfa_est_full_list <- lapply(results, function(x) x[["loadings"]])
  cfa_est_list      <- lapply(results, function(x) x[["estimates"]])
  
  if(!is.null(mi_groups)){
    
    mi_nested       <- lapply(results, function(x) x[["mi_nested"]])
    mi_fit_table    <- lapply(results, function(x) x[["mi_fit"]])
    mi_fit_list     <- lapply(results, function(x) x[["mi_fit_total"]])
    mi_fitdif_table <- lapply(results, function(x) x[["mi_fitdif"]])
    mi_fitdif_list  <- lapply(results, function(x) x[["mi_fitdif_total"]])
    
  }
  
  # Reduce results
  fit_tables <- purrr::reduce(cfa_fit_table, rbind)
  
  fit_lists  <- suppressMessages(purrr::reduce(cfa_fit_list,
                                               inner_join))
  
  est_tables <- purrr::reduce(cfa_est_list, rbind) %>% 
    dplyr::select(-op)
  
  if(!is.null(mi_groups)){
    
    grp_nested_table <- purrr::reduce(mi_nested,
                                      rbind)
    grp_fit_table    <- purrr::reduce(mi_fit,
                                      rbind)
    grp_fit_list     <- purrr::reduce(mi_fit_total,
                                      rbind)
    grp_fitdif_table <- purrr::reduce(mi_fitdif,
                                      rbind)
    grp_fitdif_list  <- purrr::reduce(mi_fitdif_list,
                                      rbind)
    
  }
  
  
  # Rename full models and estimate list
  names(cfa_models) <- names(models)
  names(cfa_est_full_list) <- paste0(names(models),
                                     "_est")
  
  
  # Compile into exportable list
  if(!is.null(mi_groups)){
    
    export <- c(
      
      "Models" = list(cfa_models),
      "Fit_Table" = list(fit_tables),
      "Fit_List"  = list(fit_lists),
      "Est_Table" = list(est_tables),
      "MI_Nested" = list(grp_nested_table),
      "MI_Fit_Table" = list(grp_fit_table),
      "MI_Fit_List" = list(grp_fit_list),
      "MI_FitDif_Table" = list(grp_fitdif_table),
      "MI_FitDif_List" = list(grp_fitdif_list),
      cfa_est_full_list
      
    )
    
  } else {
    
    export <- c(
      
      "Models" = list(cfa_models),
      "Fit_Table" = list(fit_tables),
      "Fit_List"  = list(fit_lists),
      "Est_Table" = list(est_tables),
      cfa_est_full_list
      
    )
    
  }

  
  # Return
  return(export)
  
}
