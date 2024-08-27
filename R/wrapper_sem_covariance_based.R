#' Structural Equation Modeling Wrapper
#' 
#' @description
#' A simple wrapper that automates the running and extraction of common
#' SEM fit metrics, coefficients, and fit metrics.
#' 
#' 
#' @param data A dataframe object. This should be a structured dataset where
#' each column represents a variable and each row represents an observation.
#' @param model `Lavaan` defined SEM object. Refer to \code{lavaan}
#' documentation for a description of the user-specified model.
#' @param cluster If working with clustered/multilevel/hierarchical data, define
#' the cluster or group variable to account for shared SE. Cluster should be
#' a categorical factor variable.
#' @param predictors An optional vector of character strings of the predictors
#' of interest. Doing so will filter the coefficient table to just the
#' predictors of interest for easier viewing.
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
#' model <- '
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
#' sem_wrapper(data = data,
#'             model = model)
#' 
#' @returns A list of two additional lists. The first list consists of the
#' standard \code{lavaan} SEM object. The second list is a series of dataframes
#' containing common outputs of an SEM object compiled into an exportable
#' format to an excel document. 
#' 
#' @references
#' Yves Rosseel (2012). lavaan: An R Package for Structural Equation
#' Modeling. \emph{Journal of Statistical Software}, 48(2), 1-36.
#' https://doi.org/10.18637/jss.v048.i02
#' 
#' @export
wrapper_cbsem <- function(
    data = .,
    model,
    cluster = NULL,
    predictors = NULL,
    estimator = "ML",
    se = NULL,
    missing = "listwise"
){
  
  # SEM Modeling
  sem_mod = lavaan::sem(model,
                        data = data,
                        cluster = cluster,
                        estimator = estimator,
                        se = se,
                        missing = missing,
                        fixed.x = FALSE)
  
  # Get Summary of SEM
  sem_sum <- lavaan::summary(sem_mod)
  sem_nobs <- sem_sum[["data"]][["nobs"]]
  
  if(is.null(sem_sum[["data"]][["norig"]])){
    
    sem_norig <- "-"
    
  } else{
    
    sem_norig <- sem_sum[["data"]][["norig"]]
    
  }
  
  sem_groups <- sem_sum[["data"]][["ngroups"]]
  
  # Extract goodness of fit names
  names = rownames(lavaan::fitmeasures(sem_mod) %>%
                     as.data.frame())
  
  # Get goodness of fit measures
  fittab = lavaan::fitmeasures(sem_mod) %>% 
    as.data.frame() %>% 
    rename("Value" = ".") %>% 
    rownames_to_column(var = "Metric")
  
  # Create goodness of fit table
  fittext = lavaan::fitmeasures(sem_mod) %>% 
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
  
  # Combine fit measures and fit text
  fittab = fittab %>% 
    rbind(fittext)
  
  # Get beta coefficient estimates
  esttab = as.data.frame(standardizedsolution(sem_mod)) %>%
    rename(Item = rhs) %>% 
    mutate(text = paste0("(beta = ", sprintf('%.3f', est.std),
                         ", se = ", sprintf('%.3f', se),
                         ", z = ", sprintf('%.3f', z),
                         ", 95% CI [", sprintf('%.3f', ci.lower),
                         ", ", sprintf('%.3f', ci.upper),
                         "], p ", ifelse(pvalue < .001, "< .001",
                                         paste0("= ", sprintf('%.3f',pvalue))),
                         ")"),
           fig.text = paste0(sprintf('%.3f', est.std),
                             ifelse(pvalue < .001, "***",
                                    ifelse(pvalue > .001 & pvalue < .01, "**",
                                           ifelse(pvalue > .01 & pvalue < .05, "*", ""))),
                             " [", sprintf('%.3f', ci.lower),
                             ", ", sprintf('%.3f', ci.upper),
                             "]"),
           path = paste0(Item, " -> ", lhs)) %>%
    dplyr::select(path, lhs, op, Item, label, est.std, se, ci.lower, ci.upper, everything())
  
  # Get measurement model loadings
  loadtab <- esttab %>% 
    filter(op == "=~")
  
  # Get regression coefficients
  regtab <- esttab %>% 
    filter(op == "~")
  
  # Get R^2 of outcome paths
  r2tab <- parameterestimates(sem_mod,
                              rsquare = T) %>% 
    filter(op == "r2")
  
  # Get residual correlations
  restab <- esttab %>% 
    filter(op == "~~")
  
  # Get indirect effects
  indtab <- esttab %>% 
    filter(op == ":=")
  
  
  # -------------------------------------- #
  # -- REGRESSION TABLES IN LONG FORMAT -- #
  # -------------------------------------- #
  
  reg_long <- list()
  outcomes <- unique(regtab$lhs)
  
  # Loop for every endogenous variables (e.g., mediators,)
  for (var in seq_along(outcomes)){
    
    # Get name of current outcome
    name <- outcomes[[var]]
    
    # Filter to just predictors relevant to current outcome
    pt_outcome <- regtab %>% 
      filter(lhs == name)
    
    # Simplify coefficient output to 3 digits
    pt_est = pt_outcome %>% 
      mutate(est = paste(sprintf('%.3f', est.std),
                         ifelse(pvalue > .05, "",
                                ifelse(pvalue < .05 & pvalue > .01, "*",
                                       ifelse(pvalue < .01 & pvalue > .001, "**", "***"))), sep = " ")) %>% 
      dplyr::select(Item, est)
    
    # Simplify confidence interval output
    pt_ci <- pt_outcome %>% 
      mutate(ci = paste0("(",
                         sprintf('%.3f', ci.lower),
                         ", ",
                         sprintf('%.3f', ci.upper),
                         ")"),
             Item = paste0(Item, "_ci")) %>% 
      dplyr::select(Item, ci)
    
    df1 <- c(rbind(pt_est$est, pt_ci$ci)) %>% 
      as.data.frame() %>% 
      rename(!!sym(paste0(name, " beta (95% CI)")) := ".")
    
    df2 <- c(rbind(pt_est$Item, pt_ci$Item)) %>% 
      as.data.frame() %>% 
      rename("Variable" = ".")
    
    df3 <- r2tab %>% 
      filter(lhs == name) %>% 
      dplyr::select(op, est) %>% 
      mutate(est = sprintf("%.3f", est)) %>% 
      as.data.frame() %>% 
      rename("Variable" = op,
             !!sym(paste0(name, " beta (95% CI)")) := "est") %>% 
      mutate(Variable = "R^2")
    
    df4 <- data.frame("v1" = c("n used",
                               "n total",
                               "Groups"),
                      "v2" = c(sem_nobs,
                               sem_norig,
                               sem_groups)) %>% 
      rename("Variable" = v1,
             !!sym(paste0(name, " beta (95% CI)")) := v2)
    
    pt_table <- cbind(df2, df1) %>% 
      rbind(df3) %>% 
      rbind(df4)
    
    reg_long[[var]] <- pt_table
    
  }
  
  # Reduce into workable model
  reg_long_table <- purrr::reduce(reg_long,
                                  full_join) %>% 
    mutate(Variable = ifelse(grepl("_ci", Variable), "", Variable))
  
  # Collapse the path table list into one dataframe and send R^2 to the bottom
  pt_ltab_eff <- reg_long_table %>%
    as.data.frame() %>% 
    filter(!grepl("R\\^2|^n used$|^n total$|^Groups$",
                  Variable))
  
  pt_ltab_desc <- reg_long_table %>%
    as.data.frame() %>% 
    filter(grepl("R\\^2|^n used$|^n total$|^Groups$",
                 Variable))
  
  # Get Model Numbers
  long_table <- rbind(pt_ltab_eff,
                      pt_ltab_desc) %>% 
    magrittr::set_colnames(.,
                           c("Variable",
                             rep("Beta (95% CI)", length(outcomes)))) %>% 
    rbind(names(.),
          .) %>%
    magrittr::set_colnames(.,
                           c("",
                             paste0(sprintf("Model %01d ", seq(length(outcomes))),
                                    outcomes)))
  
  
  # ------------------------------------- #
  # -- DIRECT & INDIRECT EFFECTS TABLE -- #
  # ------------------------------------- #
  
  if(!is.null(predictors)){
    
    # Get Direct Effects
    direff <- regtab %>% 
      filter(Item %in% predictors)
    
    # Combine with Indirect Effects
    if(nrow(indtab) > 0){
      
      eff_table <- direff %>% 
        rbind(indtab)
      
    } else {
      
      eff_table <- direff  
      
    }
    
  }
  
  
  # Get exportable tabs
  if(!is.null(predictors)){
    
    if(nrow(indtab) > 0){
      
      sheets = list("mod" = sem_mod,
                    "export" = list("fit" = fittab,
                                    "coef" = regtab,
                                    "resid" = restab,
                                    "indirect" = indtab,
                                    "efftable" = eff_table,
                                    "table" = long_table))
      
    } else {
      
      sheets = list("mod" = sem_mod,
                    "export" = list("fit" = fittab,
                                    "coef" = regtab,
                                    "resid" = restab,
                                    "efftable" = eff_table,
                                    "table" = long_table))
      
    }
    
  } else {
    
    if(nrow(indtab) > 0){
      
      sheets = list("mod" = sem_mod,
                    "export" = list("fit" = fittab,
                                    "coef" = regtab,
                                    "resid" = restab,
                                    "indirect" = indtab,
                                    "table" = long_table))
      
    } else {
      
      sheets = list("mod" = sem_mod,
                    "export" = list("fit" = fittab,
                                    "coef" = regtab,
                                    "resid" = restab,
                                    "table" = long_table))
      
    }
    
  }
  
  return(sheets)
  
}
