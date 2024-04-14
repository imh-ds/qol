#' MULTIPLE STRUCTURAL EQUATION MODEL WRAPPER
#' 
#' @description
#' A simple wrapper to automate the running of multiple SEMs to compare fits.
#' 
#' @param data A dataframe object. This should be a structured dataset where
#' each column represents a variable and each row represents an observation.
#' @param model A list of named \code{lavaan} defined SEM objects. Refer to \code{lavaan}
#' documentation for a description of the user-specified model.
#' @param cluster If working with clustered/multilevel/hierarchical data, define
#' the cluster or group variable to account for shared SE. Cluster should be
#' a categorical factor variable.
#' @param estimator Estimator to be used in the confirmatory factor analysis.
#' Default is maximum likelihood (ML). Alternative esimators are limited to
#' those offered by \code{lavaan}, e.g., "GLS", "WLS", "DWLS", "ULS", "DLS", and
#' "PML". Refer to \code{lavaan} documentation for additional details.
#' @param se Parameter to determine how to compute standard errors. Default
#' is set to NULL. Set to "robust" to use either "robust.sem" or
#' "robust.huber.white" depending on the estimator used. Refer to lavaan
#' documentation for additional details.
#' @param missing Parameter to determine how to handle missingness. Default
#' is set to "listwise" where all observations with missing values are deleted
#' prior to analysis. As detailed in \code{lavaan}'s documentation, this is only
#' recommended when data are missing completely at random (MCAR). Another
#' options is "ml" for full information maximum likelihood approach (fiml).
#' Refer to lavaan documentation for additional details.
#' 
#' @examples
#' # Note: Model example courtesy of 'lavaan'
#' 
#' model1 <- '
#'   # measurement model
#'     ind60 =~ x1 + x2 + x3
#'     dem60 =~ y1 + y2 + y3 + y4
#'     dem65 =~ y5 + y6 + y7 + y8
#'   # regressions
#'     dem60 ~ ind60
#'     dem65 ~ ind60 + dem60
#'   # residual correlations
#'     y1 ~~ y5
#'     y2 ~~ y4 + y6
#'     y3 ~~ y7
#'     y4 ~~ y8
#'     y6 ~~ y8
#'  '
#'  
#' model2 <- '
#'   # measurement model
#'     ind60 =~ x1 + x2
#'     dem60 =~ y1 + y2
#'     dem65 =~ y5 + y6
#'   # regressions
#'     dem65 ~ ind60 + dem60
#'  '
#' 
#' model_list <- list(First_Model = model1,
#'                    Second_Model = model2)
#' 
#' sem_table(data = data,
#'           models = model_list)
#' 
#' 
#' @references
#' Yves Rosseel (2012). lavaan: An R Package for Structural Equation
#' Modeling. \emph{Journal of Statistical Software}, 48(2), 1-36.
#' https://doi.org/10.18637/jss.v048.i02
#' 
#' @export
sem_table = function(data = .,
                     models,
                     cluster = NULL,
                     missing = "listwise",
                     se = NULL,
                     estimator = "ML"){
  
  # Create empty lists to store results
  sem_est_list = list()
  sem_fit_list = list()
  sem_fit_table = list()
  
  # Grab names of models
  mod_name_list = names(models)
  mod_reference = models
  
  # Loop
  for(mod in models){
    
    # Run SEM
    sem_model = lavaan::sem(mod,
                            data = data,
                            missing = missing,
                            se = se,
                            cluster = cluster,
                            estimator = estimator,
                            fixed.x = FALSE)
    
    mod_order = which(mod_reference == mod) %>%
      as.numeric()
    
    mod_name = mod_name_list[mod_order]
    
    names = rownames(fitmeasures(sem_model) %>%
                       as.data.frame())
    
    fittab = fitmeasures(sem_model) %>%
      as.data.frame() %>%
      data.table::transpose()
    
    colnames(fittab) = names
    
    fit_table = fittab %>% 
      mutate(Text = paste0("(chi-sq(", df,
                           ") = ", sprintf('%.3f',chisq),
                           ", CFI = ", sprintf('%.3f',cfi),
                           ", TLI = ", sprintf('%.3f',tli),
                           ", SRMR = ", sprintf('%.3f',srmr),
                           ", RSMEA = ", sprintf('%.3f',rmsea),
                           ", 90% CI [", sprintf('%.3f',rmsea.ci.lower),
                           ", ", sprintf('%.3f',rmsea.ci.upper),
                           "])")) %>% 
      dplyr::select(Text, everything())
    
    sem_fit_table[[mod_name]] = matrix(NA,
                                       nrow = 1,
                                       ncol = 10)
    
    colnames(sem_fit_table[[mod_name]]) = c("MODEL",
                                            "X2",
                                            "df",
                                            "CFI",
                                            "TLI",
                                            "RMSEA",
                                            "LOWER",
                                            "UPPER",
                                            "SRMR",
                                            "ECVI")
    
    sem_fit_table[[mod_name]][1, ] = c(paste(mod_name), 
                                       sprintf('%.3f',
                                               lavaan::fitmeasures(sem_model,
                                                                   c("chisq", "df", "cfi", "tli", "rmsea",
                                                                     "rmsea.ci.lower", "rmsea.ci.upper",
                                                                     "srmr", "ecvi"))))
    
    sem_fit_list[[paste0(mod_name,"_fit")]] = fit_table %>%
      as.data.frame()
    
    sem_est_list[[paste0(mod_name,"_est")]] = as.data.frame(lavaan::standardizedsolution(sem_model)) %>%
      rename(Item = rhs)
    
  }
  
  fit_tables = sem_fit_table %>%
    purrr::reduce(rbind) %>%
    as.data.frame()
  
  fit_lists = sem_fit_list %>%
    purrr::reduce(rbind) %>%
    as.data.frame() %>% 
    rownames_to_column(var = "Model") %>% 
    mutate(Model = names(models))
  
  table_full = c("Fit_Table" = list(fit_tables),
                 "Fit_List" = list(fit_lists),
                 sem_est_list)
  
  return(table_full)
  
}
