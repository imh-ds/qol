#' Exploratory Factor Analysis Wrapper
#' 
#' @description
#' Runs exploratory factor analysis (EFA) through the \code{psych} package's \code{fa()}
#' function. A simple wrapper that automates a general EFA for most needs.
#' For complex or custom settings, users are recommended to simply use the
#' \code{psych::fa} (Revelle, 2023). 
#' 
#' @param data A dataframe object. This should be a structured dataset where
#' each column represents a variable and each row represents an observation.
#' @param variables A required vector of column names representing their 
#' manifest variables.
#' @param dictionary variable dictionary
#' @param factors_n A single numeric value for the number of factors to
#' identify. Default is NULL where it automatically determines the optimal
#' number of factors.
#' @param rotation Defaults to "oblimin" for an oblique rotation. See
#' \code{help(psych::fa)} for other rotation options.
#' @param missing Defaults to NULL, and does not impute missing values. If
#' set to TRUE, specify impute option.
#' @param impute default to "none". Specify impute option "mean" or "median" to replace missing values.
#' @param plot default to FALSE. If set to TRUE, returns elbow plot.
#' 
#' @return The exploratory factor analysis output, a dataframe of the items
#' sorted from highest to lowest factor loading on their respective factors,
#' and a list of additional outputs.
#' 
#' @examples
#' variables <- c("item1", "item2", "item3", "item4")
#' efa_wrapper(data, variables, plot = TRUE)
#' 
#' @references William Revelle (2023). psych: Procedures for Psychological,
#' Psychometric, and Personality Research. Northwestern University, Evanston,
#' Illinois. R package version 2.3.3,
#' \url{https://CRAN.R-project.org/package=psych}.
#' 
#' @export
wrap_efa <- function(
    
    data = .,
    vars,
    codebook = NULL,
    factors_n = NULL,
    rotation = "oblimin",
    missing = FALSE,
    impute = "none",
    plot = FALSE
    
){
  
  # Get working data
  df <- data %>% 
    dplyr::select(dplyr::all_of(vars))
  
  
  # If n number of factors are not specified, automatically grab best factor n.
  # Otherwise, run only desired factor n.
  if (is.null(factors_n)) {
    
    recommended_n <- psych::fa.parallel(df,
                                        fm = "minres",
                                        plot = plot)[["nfact"]]
    
    efa <- psych::fa(
      r = df,
      fm = "minres",
      nfactors = recommended_n,
      rotate = rotation,
      missing = missing,
      impute = impute
    )
    
  } else {
    
    efa <- psych::fa(
      r = df,
      fm = "minres",
      nfactors = factors_n,
      rotate = rotation,
      missing = missing,
      impute = impute
    )
    
  }
  
  
  # Sort EFA
  efa <- psych::fa.sort(efa)
  
  
  # Get EFA fit metrics
  efa_stats <- tibble::tribble(
    
    ~metric, ~value,
    "Mean Item Complexity", mean(efa[["complexity"]]),
    "Total", NA,
    "Observations", efa[["n.obs"]],
    "Likelihood \u03C7\u00B2", efa[["STATISTIC"]],
    "Probability < ", efa[["PVAL"]],
    "Harmonic", NA,
    "Observations", efa[["nh"]],
    "Empirical \u03C7\u00B2", efa[["chi"]],
    "Probability < ", efa[["EPVAL"]],
    "Null Model", NA,
    "DF", efa[["null.dof"]],
    "Objective Function", efa[["null.model"]],
    "\u03C7\u00B2", efa[["null.chisq"]],
    "Tested Model", NA,
    "DF", efa[["dof"]],
    "Objective Function", efa[["objective"]],
    "Fit", NA,
    "Root Mean Square of Residuals (RMSR)", efa[["rms"]],
    "Corrected Root Mean Square of Residuals (CRMSR)", efa[["crms"]],
    "Tucker Lewis Index (TLI)", efa[["TLI"]],
    "Root Mean Square Error of Approximation (RMSEA)", efa[["RMSEA"]][[1]],
    paste0("RMSEA ", round(efa[["RMSEA"]][[4]] * 100), "% CI Lower"), efa[["RMSEA"]][[2]],
    paste0("RMSEA ", round(efa[["RMSEA"]][[4]] * 100), "% CI Upper"), efa[["RMSEA"]][[3]],
    "Bayesian Information Criterion (BIC)", efa[["BIC"]]
    
  ) %>% 
    as.data.frame()
  
  
  
  # Get EFA stats; uniqueness, communality, & complexity
  efa_ucc <- data.frame(
    
    uniqueness = efa[["uniquenesses"]],
    communality = efa[["communality"]],
    complexity = efa[["complexity"]]
    
  ) %>% 
    
    # Rename row names to item
    tibble::rownames_to_column(var = "item")
  
  
  
  # Get EFA Variance
  efa_var <- as.data.frame(
    
    efa[["Vaccounted"]]
    
  ) %>% 
    
    # Rename columns
    magrittr::set_colnames(
      .,
      sprintf("fac%02d", seq(ncol(.)))
    ) %>% 
    
    # Get rownames as explanation
    tibble::rownames_to_column(var = "measure")
  
  
  
  # Get EFA Loadings
  efa_loadings <- as.data.frame(
    
    efa[["loadings"]][]
    
  ) %>% 
    
    # Rename columns
    magrittr::set_colnames(
      .,
      sprintf("fac%02d", seq(ncol(.)))
    ) %>% 
    
    # Get rownames as item
    tibble::rownames_to_column(var = "item")
    
  
  
  # EFA Loadings Table
  efa_tab <- suppressMessages(
    
    dplyr::inner_join(efa_loadings,
                      efa_ucc)
    
  )
  
  
  
  # If codebook is given, then combine
  if(!is.null(codebook)){
    
    efa_tab <- suppressMessages(
      
      dplyr::inner_join(
        efa_tab,
        codebook
      )
      
    ) %>% 
      
      # Reorder
      dplyr::relocate(
        label,
        .before = fac01
      )
    
  }
  
  
  # Factor Scores
  efa_scores <- as.data.frame(efa[["scores"]]) %>% 
    magrittr::set_colnames(.,
                           sprintf("fac%02d", 
                                   seq(ncol(.))))
  
  # Factor Weights
  efa_weights <- as.data.frame(efa[["weights"]]) %>% 
    magrittr::set_colnames(.,
                           sprintf("fac%02d", 
                                   seq(ncol(.)))) %>% 
    tibble::rownames_to_column(var = "item")
  
  
  # Compile results
  fa_results <- list(
    
    efa <- efa,
    loadings <- efa_tab,
    metrics <- efa_stats,
    variance <- efa_var,
    fa_scores <- efa_scores,
    fa_weights <- efa_weights
    
  )
  
  # Return
  return(fa_results)
  
}
