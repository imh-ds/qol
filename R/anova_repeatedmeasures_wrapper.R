#' Repeated Measures Analysis of Variance (ANOVA) Wrapper
#'
#' @description A wrapper for running Repeated Measures Analysis of Variance
#'   (ANOVA).
#'
#' @param data A dataframe object. This should be a structured dataset where
#'   each column represents a variable and each row represents an observation.
#' @param key An optional character to specify the name of the repeated
#'   measures.
#' @param value An optional character to specify the name of the scores of the
#'   repeated measures.
#' @param within_vars Characters indicating the repeated measures variables.
#' @param between_vars An optional character indicating the between groups
#'   factor grouping variable.
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
#'
#' @export
wrap_rm_anova <- function(
    
  data,
  key = "within",
  value = "value",
  id,
  within_vars,
  between_vars = NULL,
  covariates = NULL,
  interaction = FALSE,
  conf_level = 0.95,
  ph_method = "holm",
  es_type = "hedges"
  
) {
  
  # WARNINGS & MESSAGES -----------------------------------------------------
  
  # If covariates is specified
  if (!is.null(covariates)) {
    
    message(
      "Detecting covariates. Running repeated measures ANCOVA."
    )
    
  }
  
  # If interaction is TRUE and between_vars is specified
  if (isTRUE(interaction) && !is.null(between_vars)) {
    
    message(
      paste(
        "Exact interaction terms not specified.",
        "Running interactions across all groups."
      )
    )
    
  }
  
  # If interaction is TRUE but between_vars is not specified
  if (isTRUE(interaction) && is.null(between_vars)) {
    
    message(
      "Interaction specified as TRUE but no between subjects variables are specified. Reverting interaction to FALSE."
    )
    
    interaction <- FALSE
    
  }
  
  
  # BASIC DATA PARAMETERS ---------------------------------------------------
  
  
  # Define key
  if (is.null(key)) {
    key <- "measures"
  }
  
  # Define value
  if (is.null(value)) {
    value <- "value"
  }
  
  
  # Define interaction effects
  # Specify interaction terms if any
  if (isFALSE(interaction)) {
    
    
    # Define ANOVA formula
    formula <- as.formula(
      
      paste0(
        
        value,
        " ~ ",
        paste(c(key,
                between_vars,
                covariates),
              collapse = " + "),
        " + Error(factor(",
        id,
        "))"
        
      )
      
    )
    
    
  } else {
    
    
    # If interaction is just specified as TRUE instead of defining specific
    # interaction, then automate every interaction across groups
    if (isTRUE(interaction)) {
      
      # Between Within Length
      bw_length <- length(c(key, between_vars))
      
      interactions_list <- lapply(
        2:bw_length,
        function(n) {
          
          combinations <- combn(c(key,
                                  between_vars), n)
          
          result <- apply(combinations,
                          2,
                          function(x) paste(x, collapse = "*"))
          
        }
        
      )
      
      interaction <- purrr::reduce(interactions_list,
                                   c)
      
    }
    
    # Define ANOVA formula
    formula <- as.formula(
      
      paste0(
        
        value,
        " ~ ",
        paste(c(key,
                between_vars,
                covariates,
                interaction),
              collapse = " + "),
        " + Error(factor(",
        id,
        "))"
        
      )
      
    )
    
    
  }
  
  # Create working data frame
  wdf <- data %>% 
    dplyr::select(
      dplyr::all_of(c(
        id,
        within_vars,
        between_vars,
        covariates)
      )
    )
  
  # Get working format dataframe
  ldf <- wdf %>%
    
    # Convert to long format
    tidyr::gather(
      key = !!key,
      value = !!value,
      -all_of(
        c(id,
          between_vars,
          covariates)
      )
    )
  
  # RUN REPEATED MEASURES ANOVA ---------------------------------------------
  
  # NOTE: Hard code the formula since `emm` cannot read formula objects.
  aov_result <- do.call(
    aov,
    list(formula,
         data = ldf)
  )
  
  # Between subjects result
  aov_result_bs <- aov_result[[paste0("factor(",
                                      id,
                                      ")")]]
  
  # Within subjects result
  aov_result_ws <- aov_result[["Within"]]
  
  
  
  # RUN ASSUMPTIONS TESTS ---------------------------------------------------
  
  # Between Subjects
  if (!is.null(between_vars)) {
    
    aov_assumptions_bs <- calc_aov_assumptions(
      data = ldf,
      outcome = value,
      groups = between_vars,
      aov_result = aov_result_bs
    )
    
  }
  
  # Within Subjects
  aov_assumptions_ws <- calc_aov_assumptions(
    data = ldf,
    outcome = value,
    groups = key,
    aov_result = aov_result_ws
  )
  
  
  # COMPILE SUMMARY OF ANOVA RESULTS ----------------------------------------
  
  # Between Subjects
  if (!is.null(between_vars) | !is.null(covariates)) {
    
    aov_summary_bs <- calc_aov_summary(
      
      aov_model = aov_result_bs
      
    )
    
  }
  
  # Within Subjects
  aov_summary_ws <- calc_aov_summary(
    
    aov_model = aov_result_ws
    
  )
  
  
  # ESTIMATED MARGINAL MEANS ------------------------------------------------
  
  # Between subjects EMM
  if (!is.null(between_vars)) {
    
    emm_list_bs <- calc_aov_emm(
      
      data = ldf,
      groups = between_vars,
      outcome = value,
      within_vars = within_vars,
      between_vars = between_vars,
      covariates = covariates,
      aov_result = aov_result,
      aov_summary = aov_summary_bs,
      conf_level = conf_level,
      ph_method = ph_method,
      es_type = es_type,
      g_correction = g_correction,
      paired = "between"
      
    )
    
  }
  
  # Within subjects EMM
  emm_list_ws <- calc_aov_emm(
    
    data = ldf,
    groups = key,
    outcome = value,
    within_vars = within_vars,
    between_vars = between_vars,
    covariates = covariates,
    aov_result = aov_result,
    aov_summary = aov_summary_ws,
    conf_level = conf_level,
    ph_method = ph_method,
    es_type = es_type,
    g_correction = g_correction,
    paired = "within"
    
  )
  
  
  # COMPILE INTO RETURNABLE RESULTS -----------------------------------------
  
  # Compile all results
  if (!is.null(between_vars)) {
    
    aov_results <- c(list(bs_summary = aov_summary_bs,
                          ws_summary = aov_summary_ws,
                          bs_norm = aov_assumptions_bs[["normality"]],
                          bs_var = aov_assumptions_bs[["variance"]],
                          ws_norm = aov_assumptions_ws[["normality"]],
                          ws_var = aov_assumptions_ws[["variance"]]),
                     emm_list_bs,
                     emm_list_ws)
    
  }
  
  if (is.null(between_vars) && !is.null(covariates)) {
    
    aov_results <- c(list(bs_summary = aov_summary_bs,
                          ws_summary = aov_summary_ws,
                          ws_norm = aov_assumptions_ws[["normality"]],
                          ws_var = aov_assumptions_ws[["variance"]]),
                     emm_list_ws)
    
  }
  
  if (is.null(between_vars) && is.null(covariates)) {
    
    aov_results <- c(list(ws_summary = aov_summary_ws,
                          ws_norm = aov_assumptions_ws[["normality"]],
                          ws_var = aov_assumptions_ws[["variance"]]),
                     emm_list_ws)
    
  }
  
  
  # Return
  return(aov_results)
  
}
