#' Analysis of Variance (ANOVA) Wrapper
#'
#' @description A wrapper for running Analysis of Variance (ANOVA) with or
#' without covariates.
#'
#' @param data A dataframe object. This should be a structured dataset where
#'   each column represents a variable and each row represents an observation.
#' @param outcome Character indicating the outcome/dependent variable.
#' @param groups Character indicating the factor grouping variable.
#' @param covariates An optional vector of variable names representing
#'   covariates of interest.
#' @param interaction Either a logical or vector of characters representing the
#'   interaction effects of interest. If specified as \code{TRUE}, all possible
#'   interactions between the groups are run (not recommended). If specified as
#'   \code{FALSE}, no interaction terms are run. If specified with interaction
#'   terms, e.g., \code{c("treatment\*gender", "treatment\*age_group",
#'   "treatment\*gender\*age_group")}, the only the specified interaction terms
#'   are run. The default is \code{FALSE}.
#' @param ph_method The post-hoc pairwise comparison correction method. Options
#'   include \code{"tukey"}, \code{"scheffe"} , \code{"sidak"},
#'   \code{"bonferroni"}, \code{"dunnettx"}, \code{"mvt"}, and \code{"none"}.
#'   The default is \code{"tukey"}. See below for additional information.
#' @param conf_level Numeric value reflecting the confidence interval level. The
#'   default is 0.95 for 95% confidence interval.
#' @param es_type Character indicating which effect size to return. Options
#'   include \code{"cohen"} for Cohen's d, \code{"hedges"} for Hedges' g, or
#'   \code{"glass"} for Glass's delta. The default is Hedges' g.
#' @param g_correction Character indicating which Hedges' g correction to apply.
#'   Options include \code{"standard"} for regular Hedges' g correction and
#'   \code{"log"} to apply logarithmic Hedges' g correction. The default is
#'   standard.
#'
#'
#' @export
wrap_anova <- function(
    
  data,
  outcome,
  groups,
  covariates = NULL,
  interaction = FALSE,
  ph_method = "tukey",
  conf_level = .95,
  es_type = "hedges",
  g_correction = "standard"
  
) {
  
  
  # WARNINGS & MESSAGES -----------------------------------------------------
  
  # If covariates is specified
  if (!is.null(covariates)) {
    
    message(
      "Detecting covariates. Running ANCOVA."
    )
    
  }
  
  # If interaction is TRUE
  if (isTRUE(interaction)) {
    
    message(
      paste(
        "Exact interaction terms not specified.",
        "Running interactions across all groups."
      )
    )
    
  }
  
  
  
  # BASIC DATA PARAMETERS ---------------------------------------------------
  
  
  # Change groups to factors
  for(g in groups){
    
    # Automatically detect if variable has only numerics. If so, recode to add
    # "L" as a prefix for "Level".
    if (all(grepl("^\\d+$", 
                  data[[g]], 
                  perl = TRUE) | is.na(data[[g]]))) {
      
      # Add "L" in front
      data[[g]] <- ifelse(!is.na(data[[g]]), 
                          paste0("L", 
                                 data[[g]]), 
                          data[[g]])
      
    }
    
    # If the group variable has spaces, recode to underscore "_".
    if (any(grepl(" ",
                  data[[g]],
                  fixed = TRUE))) {
      
      data[[g]] <- gsub(" ",
                        "_",
                        data[[g]])
      
    }
    
    # If not already a factor, then factorize the group variables.
    if (!is.factor(data[[g]])) {
      
      data[[g]] <- as.factor(data[[g]])
      
    }
    
  }
  
  
  # Specify interaction terms if any
  if (isFALSE(interaction)) {
    
    # Define ANOVA formula
    formula <- as.formula(
      paste0(outcome,
             " ~ ",
             paste0(c(groups,
                      covariates),
                    collapse = " + ")))
    
    # Define EMM formula
    em_formula <- as.formula(
      paste0(" ~ ",
             paste0(c(groups,
                      covariates),
                    collapse = " + ")))
    
  } else {
    
    # If interaction is just specified as TRUE instead of defining specific
    # interaction, then automate every interaction across groups
    if (isTRUE(interaction)) {
      
      interactions_list <- lapply(
        2:length(groups),
        function(n) {
          
          combinations <- combn(groups, n)
          
          result <- apply(combinations,
                          2,
                          function(x) paste(x, collapse = "*"))
          
        }
        
      )
      
      int_combos <- purrr::reduce(interactions_list,
                                  c)
      
    }
    
    # Define ANOVA formula
    formula <- as.formula(
      paste0(outcome,
             " ~ ",
             paste0(c(groups,
                      covariates,
                      int_combos),
                    collapse = " + ")))
    
    # Define EMM formula
    em_formula <- as.formula(
      paste0(" ~ ",
             paste0(c(groups,
                      covariates,
                      int_combos),
                    collapse = " + ")))
    
  }
  
  
  
  # RUN ANOVA ---------------------------------------------------------------
  
  # Perform the ANOVA
  aov_result <- aov(formula = formula, 
                    data = data)
  
  
  # RUN ASSUMPTION TESTS ----------------------------------------------------
  
  aov_assumptions <- calc_aov_assumptions(
    data = data,
    outcome = outcome,
    aov_result = aov_result
  )
  
  
  # COMPILE SUMMARY OF ANOVA RESULTS ----------------------------------------
  
  # Get ANOVA summary
  aov_summary <- calc_aov_summary(
    
    aov_model = aov_result
    
  )
  
  # ANOVA Summary update with overall model
  aov_summary <- calc_aov_overall(
    
    aov_summary
    
  )
  
  
  # ESTIMATED MARGINAL MEANS ------------------------------------------------
  
  emm_list <- calc_aov_emm(
    
    data = data,
    groups = groups,
    outcome = outcome,
    aov_result = aov_result,
    aov_summary = aov_summary,
    conf_level = conf_level,
    ph_method = ph_method,
    es_type = es_type,
    g_correction = g_correction
    
  )
  
  
  # COMPILE INTO RETURNABLE RESULTS -----------------------------------------
  
  # Compile all results
  aov_results <- c(list(summary = aov_summary,
                        norm = aov_assumptions[["normality"]],
                        var = aov_assumptions[["variance"]]),
                   emm_list)
  
  # Return
  return(aov_results)
  
}
