
#' Calculate Wilcoxon Rank-Sum / Mann-Whitney U Test
#'
#' @description
#' Calculate Wilcoxon Rank-Sum / Mann-Whitney U independent samples t-test. 
#' 
#' @param outcome Character indicating the outcome/dependent variable.
#' @param group Character indicating the binary factor grouping variable.
#' @param data A dataframe object. This should be a structured dataset where
#'   each column represents a variable and each row represents an observation.
#' @param alternative Character indicating the direction of the alternative
#'   hypothesis. Options include \code{"two.sided"}, \code{"greater"}, or
#'   \code{"less"}. The default is two sided alternative hypothesis.
#' @param conf_level Numeric value reflecting the confidence interval level. The
#'   default is 0.95 for 95% confidence interval.
#' 
#' @export
calc_wilcox_test <- function(
    
  outcome,
  group,
  data,
  alternative = "two.sided",
  conf_level = 0.95
  
) {
  
  # Set formula
  formula <- as.formula(
    paste0(outcome,
           " ~ ",
           group)
  )
  
  
  # Run t-test
  mw_test <- wilcox.test(
    
    formula = formula,
    data = data,
    alternative = alternative,
    conf.int = TRUE,
    conf.level = conf_level
    
  )
  

  # GET MODEL PARAMETERS ----------------------------------------------------
  
  # Get degrees of freedom
  df   <- NA
  
  # Get Hodges-Lehmann estimate
  h <- mw_test[["estimate"]][[1]]
  
  # Get Hodges-Lehmann confidence interval
  h_ci_lower <- mw_test[["conf.int"]][[1]]
  h_ci_upper <- mw_test[["conf.int"]][[2]]
  
  # Get test statistic
  stat <- mw_test[["statistic"]][[1]]
  
  # Get standard error
  se <- NA
  

  # GET EFFECT SIZE ---------------------------------------------------------
  
  # Get rank-biserial correlation
  rbc  <- effectsize::rank_biserial(
    
    x = formula,
    data = data,
    alternative = alternative,
    ci = conf_level
    
  )
  
  c <- rbc[["r_rank_biserial"]]
  c_ci_lower <- rbc[["CI_low"]]
  c_ci_upper <- rbc[["CI_high"]]
  
  # Calculate rank-biserial SE
  n <- length(data[[outcome]])
  se_rbc <- sqrt((1 - c^2) / (n - 2))
  
  # Compile into t-test data frame
  mw_df <- data.frame(
    
    test = "Wilcoxon Rank-Sum",
    stat = stat,
    df = df,
    p = mw_test[["p.value"]],
    m_dif = h,
    se_dif = se,
    m_ci_lower = h_ci_lower,
    m_ci_upper = h_ci_upper,
    es_type = "Rank Biserial Correlation",
    es = c,
    es_se = se_rbc,
    es_ci_lower = c_ci_lower,
    es_ci_upper = c_ci_upper
    
  )
  
  # Return
  return(mw_df)
  
}