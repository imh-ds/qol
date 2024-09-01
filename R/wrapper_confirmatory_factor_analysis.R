#' Confirmatory Factor Analysis Wrapper
#'
#' @description This function conducts a confirmatory factor analysis (CFA)
#'   using the \code{lavaan} package (Rosseel, 2012). It automatically compiles
#'   and returns tables of commonly reported results. The function supports both
#'   regular CFA and measurement invariance (MI).
#'
#' @param data A data frame. Each column represents a variable and each row an
#'   observation.
#' @param model A CFA model compatible with \code{lavaan}. See \code{lavaan}
#'   documentation for instructions on specifying a CFA model.
#' @param name An optional name for the CFA model. Defaults to 'CFA Model' if
#'   not specified.
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
#'   Other estimators are those offered by \code{lavaan}, e.g., \code{"GLS"},
#'   \code{"WLS"}, \code{"DWLS"}, \code{"ULS"}, \code{"DLS"}, and \code{"PML"}.
#'   See \code{lavaan} documentation for more details.
#' @param digits An integer specifying the number of decimal places for rounding
#'   in the in-text reference generator.
#' @param standardized A logical value indicating whether to return standardized
#'   or unstandardized estimates. If \code{TRUE}, the function returns
#'   standardized estimates. If \code{FALSE}, it returns unstandardized
#'   estimates. Default is \code{TRUE}.
#' @param mi_groups A vector of column names representing the categorical group
#'   variables for testing measurement invariance. Default is \code{NULL} which
#'   means measurement invariance is not run. If specified, the function will
#'   run measurement invariance on the groups and return fit metrics.
#'
#' @return A list containing a \code{lavaan} CFA model and data frames with the
#'   results of the analysis. The basic output includes data frame tables for
#'   goodness of fit metrics and model estimations. If measurement invariance
#'   groups were specified, fit indices and goodness of fit comparison tables
#'   are also included in the output.
#'
#' @examples
#' model <- 'anxiety =~ x1 + x2 + x3
#'           sleep   =~ z1 + z2'
#' cfa_wrapper(data, model, mi_groups = c("gender", "ethnicity"))
#'
#'
#' @references Yves Rosseel (2012). lavaan: An R Package for Structural Equation
#'   Modeling. \emph{Journal of Statistical Software}, 48(2), 1-36.
#'   \url{https://doi.org/10.18637/jss.v048.i02}.
#'
#' @export
wrap_cfa <- function(
    
  data = .,
  model,
  name = NULL,
  cluster = NULL,
  missing = "listwise",
  se = NULL,
  bootstrap = 1000,
  estimator = "ML",
  digits = 3,
  standardized = TRUE,
  mi_groups = NULL
  
) {
  

  # RUN CONFIRMATORY FACTOR ANALYSIS ----------------------------------------
  
  # Run Confirmatory Factor Analysis
  cfa_model <- lavaan::cfa(
    
    model = model,
    data = data,
    missing = missing,
    cluster = cluster,
    se = se,
    bootstrap = bootstrap,
    estimator = estimator
    
  )
  
  
  # Get model name
  if(is.null(name)){
    
    mod_name <- "CFA Model"
    
  } else {
    
    mod_name <- name
    
  }
  
  
  # Get fit metrics
  fit_metrics <- data.frame(
    lavaan::fitmeasures(cfa_model)
  ) %>% 
    
    # Rename columns
    magrittr::set_colnames(.,
                           mod_name) %>% 
    
    # Create fit metric variable
    tibble::rownames_to_column(var = "fit_metric")
  
  
  # Get fit metrics table
  fit_df <- data.frame(
    t(fit_metrics)
  ) %>% 
    
    # Convert first row to column names
    janitor::row_to_names(
      row_number = 1
    ) %>%
    
    # Transform all variables back to numeric
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::everything(),
        \(x) as.numeric(x)
      )
    ) %>% 
    
    # Create model variable
    tibble::rownames_to_column(var = "model")
  
  
  # Rounding setting
  rnd <- paste0("%.",
                digits,
                "f")
  

  # CFA FIT INDICES ---------------------------------------------------------
  
  # Get individual fit metrics
  
  # Chi-square
  chisq <- fit_df[["chisq"]]
  
  # Degrees of Freedom
  df <- fit_df[["df"]]
  
  # Comparative Fit Index
  cfi <- fit_df[["cfi"]]
  
  # Tucker Lewis Index
  tli <- fit_df[["tli"]]
  
  # Root Mean Square Error of Approximation
  rmsea <- fit_df[["rmsea"]]
  
  # Root Mean Square Error of Approximation Lower Bound of CI
  rmsea_lower <- fit_df[["rmsea.ci.lower"]]
  
  # Root Mean Square Error of Approximation Upper Bound of CI
  rmsea_upper <- fit_df[["rmsea.ci.upper"]]
  
  # Standardized Root Mean Square Residual
  srmr <- fit_df[["srmr"]]
  
  # Expected Cross Validation Index
  ecvi <- fit_df[["ecvi"]]
  
  # Create fit in-text reference
  fit_text <- paste0(
    
    "(chi-sq(", df,
    ") = ", sprintf(rnd,
                    chisq),
    ", CFI = ", sprintf(rnd,
                        cfi),
    ", TLI = ", sprintf(rnd,
                        tli),
    ", SRMR = ", sprintf(rnd,
                         srmr),
    ", RSMEA = ", sprintf(rnd,
                          rmsea),
    ", 90% CI [", sprintf(rnd,
                          rmsea_lower),
    ", ", sprintf(rnd,
                  rmsea_upper),
    "])"
    
  )
  
  # Save to CFA Fit Table object
  cfa_fit_table <- data.frame(
    
    model = mod_name,
    x2 = chisq,
    df = df,
    cfi = cfi,
    tli = tli,
    rmsea = rmsea,
    rmsea_lower = rmsea_lower,
    rmsea_upper = rmsea_upper,
    srmr = srmr,
    ecvi = ecvi,
    text = fit_text
    
  )
  

  # CFA LOADINGS ------------------------------------------------------------
  
  # Grab CFA standardized loadings if standardized == TRUE
  if(isTRUE(standardized)){
    
    cfa_loadings <- data.frame(
      lavaan::standardizedsolution(cfa_model)
    )
    
    cfa_estimates <- cfa_loadings %>% 
      dplyr::filter(op == "=~") %>% 
      dplyr::rename(est = est.std)
    
  }
  
  # Grab CFA loadings if standardized == FALSE
  if(isFALSE(standardized)){
    
    cfa_loadings <- data.frame(
      lavaan::parameterestimates(cfa_model)
    )
    
    cfa_estimates <- cfa_loadings %>% 
      dplyr::filter(op == "=~")
    
  }
  
  
  # Grab vector of unique latent variables
  cfa_unique_latents <- base::unique(cfa_estimates[["lhs"]])
  
  
  # Apply the estimate calculation to each unique latent variable
  cfa_estimates_list <- lapply(
    cfa_unique_latents,
    function(u) {
      
      calculate_cfa_est(
        u = u,
        estimates = cfa_estimates
      )
      
    }
  )
  
  
  # Reduce to estimates table
  cfa_estimates_table <- purrr::reduce(
    cfa_estimates_list,
    rbind
  ) %>% 
    
    dplyr::mutate(
      model = c(mod_name, 
                rep(NA, 
                    nrow(.)-1))
    ) %>% 
    dplyr::select(
      model, 
      dplyr::everything()
    )
  
  

  # MEASUREMENT INVARIANCE --------------------------------------------------
  
  # If measurement invariance is specified
  if(!is.null(mi_groups)){
    
    # Create specific summary function
    summary.FitDiff <- function(object){
      
      export <- list(
        
        nested = object@nested,
        fit = object@fit,
        fit.diff = object@fit.diff
        
      )
      
      return(export)
      
    }
    
    # Create MI lists
    mi_results <- lapply(
      
      mi_groups,
      function(group) {
        
        # Configural Invariance
        Configural <- lavaan::cfa(
          
          model = model,
          data = data,
          missing = missing,
          cluster = cluster,
          group = group,
          se = se,
          bootstrap = bootstrap,
          estimator = estimator
          
        )
        
        # Metric Invariance
        Metric <- lavaan::cfa(
          
          model = model,
          data = data,
          missing = missing,
          cluster = cluster,
          group = group,
          se = se,
          bootstrap = bootstrap,
          estimator = estimator,
          group.equal = c("loadings")
          
        )
        
        # Scalar Invariance
        Scalar <- lavaan::cfa(
          
          model = model,
          data = data,
          missing = missing,
          cluster = cluster,
          group = group,
          se = se,
          bootstrap = bootstrap,
          estimator = estimator,
          group.equal = c("loadings",
                          "intercepts")
          
        )
        
        # Strict Invariance
        Strict <- lavaan::cfa(
          
          model = model,
          data = data,
          missing = missing,
          cluster = cluster,
          group = group,
          se = se,
          bootstrap = bootstrap,
          estimator = estimator,
          group.equal = c("loadings",
                          "intercepts",
                          "residuals")
          
        )
        
        # Compare across invariances
        compare <- semTools::compareFit(
          
          Configural,
          Metric,
          Scalar,
          Strict
          
        )
        
        # Create summary object
        comparesum <- summary(compare)
        
        # Chi-sq differences across groups
        nested_sums <- data.frame(comparesum[["nested"]]) %>% 
          magrittr::set_colnames(., tolower(names(.))) %>% 
          dplyr::rename("p.chisq" = "pr..chisq.") %>% 
          tibble::rownames_to_column(var = "invariance") %>% 
          dplyr::mutate(group = c(group,
                                  rep(NA,3))) %>% 
          dplyr::select(group, 
                        invariance,
                        chisq,
                        chisq.diff,
                        df,
                        df.diff,
                        p.chisq,
                        rmsea,
                        aic,
                        bic)
        
        # Get all fit metrics across invariances
        fit_total_sums <- data.frame(comparesum[["fit"]]) %>% 
          tibble::rownames_to_column(var = "invariance") %>% 
          dplyr::mutate(group = c(group,
                                  rep(NA,3))) %>% 
          dplyr::select(group,
                        dplyr::everything())
        
        # Get most relevant fit metrics and create reportable table
        fit_table_sums <- fit_total_sums %>% 
          dplyr::select(group,
                        invariance,
                        chisq,
                        df,
                        pvalue,
                        cfi,
                        tli,
                        rmsea,
                        rmsea.ci.lower,
                        rmsea.ci.upper,
                        rmsea.pvalue,
                        srmr,
                        ecvi,
                        aic,
                        bic)
        
        # Get all fit difference metrics across invariances
        fitdif_sums <- data.frame(comparesum[["fit.diff"]]) %>% 
          tibble::rownames_to_column(var = "comparison") %>% 
          dplyr::mutate(group = c(group,
                                  rep(NA,2))) %>% 
          dplyr::select(group,
                        dplyr::everything())
        
        # Get relevant fit metrics into reportable table
        fitdif_table_sums <- fitdif_sums %>% 
          dplyr::select(group,
                        comparison,
                        cfi,
                        tli,
                        rmsea,
                        srmr,
                        ecvi,
                        aic,
                        bic)
        
        # Return list of MI results
        return(
          
          list(
            
            nested_sums = nested_sums,
            fit_total_sums = fit_total_sums,
            fit_table_sums = fit_table_sums,
            fitdif_sums = fitdif_sums,
            fitdif_table_sums = fitdif_table_sums
            
          )
        )
        
      }
      
    )
    
    # Extract the results
    nested_sums       <- lapply(mi_results, 
                                function(x) x[["nested_sums"]])
    fit_total_sums    <- lapply(mi_results, 
                                function(x) x[["fit_total_sums"]])
    fit_table_sums    <- lapply(mi_results, 
                                function(x) x[["fit_table_sums"]])
    fitdif_sums       <- lapply(mi_results, 
                                function(x) x[["fitdif_sums"]])
    fitdif_table_sums <- lapply(mi_results, 
                                function(x) x[["fitdif_table_sums"]])
    
    # Reduce tables into dataframes
    nested_table        <- purrr::reduce(nested_sums, 
                                         rbind)
    fit_total_table     <- purrr::reduce(fit_total_sums, 
                                         rbind)
    fit_table           <- purrr::reduce(fit_table_sums, 
                                         rbind)
    fitdif_total_table  <- purrr::reduce(fitdif_sums, 
                                         rbind)
    fitdif_table        <- purrr::reduce(fitdif_table_sums, 
                                         rbind)
    
  }
  
  
  # Compile into exportable list
  if(!is.null(mi_groups)){
    
    results <- list(
      
      model = cfa_model,
      fit_table = cfa_fit_table,
      fit_table_full = fit_metrics,
      estimates = cfa_estimates_table,
      loadings = cfa_loadings,
      mi_nested = nested_table,
      mi_fit = fit_table,
      mi_fit_total = fit_total_table,
      mi_fitdif = fitdif_table,
      mi_fitdif_total = fitdif_total_table
      
    )
    
  } else {
    
    results <- list(
      
      model = cfa_model,
      fit_table = cfa_fit_table,
      fit_table_full = fit_metrics,
      estimates = cfa_estimates_table,
      loadings = cfa_loadings
      
    )
    
  }
  
  # Return
  return(results)
  
}
