#' Analysis of Variance (ANOVA) Wrapper
#'
#' @param data A dataframe object. This should be a structured dataset where
#'   each column represents a variable and each row represents an observation.
#' @param outcome A required character indicating the outcome/dependent variable
#'   when running one-way ANOVA.
#' @param between_vars Required character(s) indicating the factor grouping
#'   variable(s) when running one-way ANOVA. Optional when running
#'   repeated-measures ANOVA.
#' @param within_vars Required characters indicating the within-subjects or
#'   repeated variables when running repeated-measures ANOVA.
#' @param rm An optional character to specify the name of the repeated
#'   measures.
#' @param value An optional character to specify the name of the scores of the
#'   repeated measures.
#' @param id A required character indicating the observation ID variable when
#'   running repeated-measures ANOVA.
#' @param covariates An optional vector of variable names representing
#'   covariates of interest.
#' @param interactionss Either a logical or vector of characters representing the
#'   interactions effects of interest. If specified as \code{TRUE}, all possible
#'   interactionss between the groups are run (not recommended). If specified as
#'   \code{FALSE}, no interactions terms are run. If specified with interactions
#'   terms, e.g., \code{c("treatment\*gender", "treatment\*age_group",
#'   "treatment\*gender\*age_group")}, the only the specified interactions terms
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
wrap_anova <- function(
    
  data,
  outcome = NULL,
  between_vars = NULL,
  within_vars = NULL,
  rm = "within",
  value = "value",
  id = NULL,
  covariates = NULL,
  interactions = FALSE,
  rm_correction = "auto",
  posthoc_correction = "tukey",
  conf_level = 0.95,
  es_type = "hedges",
  emm_weights = TRUE
  
) {
  
  
  # ERROR MESSAGES ----------------------------------------------------------
  
  # If both between_vars and within_vars are missing
  if (is.null(between_vars) && is.null(within_vars)) {
    
    stop("Both between_vars and within_vars cannot be NULL.")
    
  }
  
  # If within-subjects but ID is missing
  if (!is.null(within_vars) && is.null(id)) {
    
    stop("ID cannot be NULL if running within-subjects ANOVA. Please specify id.")
    
  }
  
  # If between-subjects but outcome is missing
  if (!is.null(between_vars) && is.null(within_vars) && is.null(outcome)) {
    
    stop("Outcome cannot be NULL if running between-subjects ANOVA. Please specify outcome.")
    
  }
  
  
  
  # RUN ANOVA ---------------------------------------------------------------
  
  # If no repeated measures are specified, run between-subjects ANOVA
  if (is.null(within_vars)) {
    
    anova <- calc_anova(
      
      data = data,
      outcome = outcome,
      between_vars = between_vars,
      covariates = covariates,
      interactions = interactions,
      posthoc_correction = posthoc_correction,
      es_type = es_type,
      conf_level = conf_level,
      emm_weights = emm_weights
      
    )
    
  }
  
  
  # If repeated measures are specified, run within-subjects ANOVA
  if (!is.null(within_vars)) {
    
    anova <- calc_rm_anova(
      
      data = data,
      rm = rm,
      value = value,
      id = id,
      within_vars = within_vars,
      between_vars = between_vars,
      covariates = covariates,
      interactions = interactions,
      correction = rm_correction,
      posthoc_correction = posthoc_correction,
      es_type = es_type,
      conf_level = conf_level,
      emm_weights = emm_weights
      
    )
    
  }
  
  
  # Return
  return(anova)
  
}
