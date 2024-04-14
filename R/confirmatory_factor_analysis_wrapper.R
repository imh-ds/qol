#' CONFIRMATORY FACTOR ANALYSIS WRAPPER
#' 
#' @description
#' Runs confirmatory factor analysis (CFA) through the \code{lavaan} package
#' (Rosseel, 2012) and automates the exporting of commonly used results. 
#' 
#' 
#' @param data A dataframe object. This should be a structured dataset where
#' each column represents a variable and each row represents an observation.
#' @param model `Lavaan` defined confirmatory factor model. Refer to \code{lavaan}
#' documentation for a description of the user-specified model.
#' @param mi_groups A required vector of column names representing the 
#' categorical group variables to test measurement invariance. Default is NULL
#' to not run measurement invariance. If specified, then the function will
#' run measurement invariance on the groups and return fit metrics.
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
#' @return A list of dataframes to be easily exported as an Excel workbook.
#' Basic output includes the goodness of fit metrics and model estimations.
#' If measurement invariance parameters were specified, then fit indices and
#' goodness of fit comparison tables are also included in the returned output.
#' 
#' @examples
#' model <- 'anxiety =~ x1 + x2 + x3
#'           sleep   =~ z1 + z2'
#' cfa_wrapper(data, model, mi_groups = c("gender", "ethnicity"))
#' 
#' 
#' @references Yves Rosseel (2012). lavaan: An R Package for Structural
#' Equation Modeling. \emph{Journal of Statistical Software}, 48(2), 1-36.
#' \url{https://doi.org/10.18637/jss.v048.i02}.
#' 
#' @export
cfa_wrapper = function(data = .,
                       model,
                       mi_groups = NULL,
                       estimator = "ML",
                       se = NULL,
                       missing = "listwise"){
  
  # Run CFA on the model
  cfa_model = lavaan::cfa(model,
                          data = df,
                          estimator = estimator,
                          se = se,
                          missing = missing)
  
  # Extract goodness of fit names
  names = rownames(lavaan::fitmeasures(cfa_model) %>% as.data.frame())
  
  # Get goodness of fit measures
  fittab = lavaan::fitmeasures(cfa_model) %>% 
    as.data.frame() %>% 
    rename("Value" = ".") %>% 
    rownames_to_column(var = "Metric")
  
  # Create goodness of fit table
  fittext = lavaan::fitmeasures(cfa_model) %>% 
    as.data.frame() %>% 
    data.table::transpose() %>% 
    magrittr::set_colnames(names) %>% 
    mutate(Value = paste0("(chi-sq(", df,
                          ") = ", sprintf('%.3f', chisq),
                          ", CFI = ", sprintf('%.3f', cfi),
                          ", TLI = ", sprintf('%.3f', tli),
                          ", SRMR = ", sprintf('%.3f', srmr),
                          ", RSMEA = ", sprintf('%.3f', rmsea),
                          ", 90% CI [", sprintf('%.3f', rmsea.ci.lower),
                          ", ", sprintf('%.3f', rmsea.ci.upper),
                          "])")) %>% 
    dplyr::select(Value) %>% 
    rownames_to_column(var = "Metric") %>% 
    mutate(Metric = "Text")
  
  fittab = fittab %>% 
    rbind(fittext)
  
  if(!is.null(mi_groups)){
    
    nested_sums = list()
    fit_total_sums = list()
    fit_table_sums = list()
    fitdif_sums = list()
    fitdif_table_sums = list()
    
    for(group in mi_groups){
      
      # Configural Invariance
      Configural = cfa(model,
                       data = df,
                       group = group,
                       missing = missing,
                       se = se,
                       estimator = estimator)
      
      # Metric Invariance
      Metric = cfa(model,
                   data = df,
                   group = group,
                   missing = missing,
                   se = se,
                   estimator = estimator,
                   group.equal = c("loadings"))
      
      # Scalar Invariance
      Scalar = cfa(model,
                   data = df,
                   group = group,
                   missing = missing,
                   se = se,
                   estimator = estimator,
                   group.equal = c("loadings", "intercepts"))
      
      # Strict Invariance
      Strict = cfa(model,
                   data = df,
                   group = group,
                   missing = missing,
                   se = se,
                   estimator = estimator,
                   group.equal = c("loadings", "intercepts", "residuals"))
      
      # Compare across invariances
      compare = semTools::compareFit(Configural,
                                     Metric,
                                     Scalar,
                                     Strict)
      
      # Create specific summary function
      summary.FitDiff = function(object){
        
        export = list(nested = object@nested,
                      fit = object@fit,
                      fit.diff = object@fit.diff)
        
        return(export)
        
      }
      
      # Create summary object
      comparesum = summary(compare)
      
      # Chi-sq differences across groups
      nested_sums[[group]] = comparesum$nested %>% 
        as.data.frame() %>% 
        rownames_to_column(var = "INVARIANCE") %>% 
        add_row(., INVARIANCE = group,
                .before = 1)
      
      # Get all fit metrics across invariances
      fit_total_sums[[group]] = comparesum$fit %>% 
        as.data.frame() %>% 
        rownames_to_column(var = "INVARIANCE") %>% 
        add_row(., INVARIANCE = group,
                .before = 1)
      
      # Get most relevant fit metrics and create reportable table
      fit_table_sums[[group]] = comparesum$fit %>% 
        select(chisq, df, pvalue, rmsea, rmsea.ci.lower, rmsea.ci.upper, rmsea.pvalue, cfi, tli, srmr, aic, bic) %>% 
        mutate(chisq = sprintf('%.3f', chisq),
               pvalue = ifelse(pvalue < 0.001, "<.001", sprintf('%.3f', pvalue)),
               rmsea = sprintf('%.3f', rmsea),
               rmsea.ci.lower = sprintf('%.3f', rmsea.ci.lower),
               rmsea.ci.upper = sprintf('%.3f', rmsea.ci.upper),
               rmsea.pvalue = ifelse(rmsea.pvalue < 0.001, "<.001", sprintf('%.3f', rmsea.pvalue)),
               cfi = sprintf('%.3f', cfi),
               tli = sprintf('%.3f', tli),
               srmr = sprintf('%.3f', srmr),
               aic = sprintf('%.3f', aic),
               bic = sprintf('%.3f', bic)) %>% 
        rownames_to_column(var = "INVARIANCE") %>% 
        add_row(., INVARIANCE = group,
                .before = 1) %>%  
        rename("χ2" = chisq,
               p = pvalue,
               RMSEA = rmsea,
               LOWER = rmsea.ci.lower,
               UPPER = rmsea.ci.upper,
               rmsea_p = rmsea.pvalue,
               CFI = cfi,
               TLI = tli,
               SRMR = srmr,
               AIC = aic,
               BIC = bic)
      
      # Get all fit difference metrics across invariances
      fitdif_sums[[group]] = comparesum$fit.diff %>% 
        as.data.frame() %>% 
        rownames_to_column(var = "INVARIANCE") %>% 
        add_row(., INVARIANCE = group,
                .before = 1)
      
      # Get relevant fit metrics into reportable table
      fitdif_table_sums[[group]] = comparesum$fit.diff %>% 
        select(rmsea, cfi, tli, srmr, aic, bic) %>% 
        mutate(rmsea = sprintf('%.3f', rmsea),
               cfi = sprintf('%.3f', cfi),
               tli = sprintf('%.3f', tli),
               srmr = sprintf('%.3f', srmr),
               aic = sprintf('%.3f', aic),
               bic = sprintf('%.3f', bic)) %>% 
        rownames_to_column(var = "COMPARISON") %>% 
        magrittr::set_colnames(toupper(names(.))) %>% 
        add_row(., COMPARISON = group,
                .before = 1)
      
    }
    
    # Reduce tables into dataframes
    nested_table = purrr::reduce(nested_sums,
                                 rbind)
    fit_total_table = purrr::reduce(fit_total_sums,
                                    rbind)
    fit_table = purrr::reduce(fit_table_sums,
                              rbind)
    fitdif_total_table = purrr::reduce(fitdif_sums,
                                       rbind)
    fitdif_table = purrr::reduce(fitdif_table_sums,
                                 rbind)
    
  }
  
  if(is.null(mi_groups)){
    
    sheets = list("GOF" = fittab,
                  "EST" = as.data.frame(standardizedsolution(cfa_model)) %>%
                    rename(Item = rhs))
    
  } else {
    
    sheets = list("GOF" = fittab,
                  "EST" = as.data.frame(standardizedsolution(cfa_model)) %>%
                    rename(Item = rhs),
                  "CHISQ" = nested_table,
                  "FIT" = fit_table,
                  "FITDIF" = fitdif_table,
                  "FIT_ALL" = fit_total_table,
                  "FITDIF_ALL" = fitdif_total_table)
    
  }
  
  return(sheets)
  
}