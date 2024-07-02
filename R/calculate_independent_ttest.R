
#' Calculate Independent Samples t-test
#' 
#' @description
#' Calculate Student's or Welch's independent samples t-test. 
#' 
#' @param data A dataframe object. This should be a structured dataset where
#'   each column represents a variable and each row represents an observation.
#' @param outcome Character indicating the outcome/dependent variable.
#' @param group Character indicating the binary factor grouping variable.
#' @param alternative Character indicating the direction of the alternative
#'   hypothesis. Options include \code{"two.sided"}, \code{"greater"}, or
#'   \code{"less"}. The default is two sided alternative hypothesis.
#' @param var_equal Logical indicating whether the groups' variances are equal
#'   or not. If \code{TRUE}, standard Student's t-test is run with the pooled
#'   variance. If \code{FALSE}, Welch's t-test approximation to the degrees of
#'   freedom is run.
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
calc_ind_ttest <- function(
    
  data = .,
  outcome,
  group,
  alternative = "two.sided",
  var_equal = FALSE,
  conf_level = 0.95,
  es_type = "hedges",
  g_correction = "standard"
  
) {
  
  # Set formula
  formula <- as.formula(
    paste0(outcome,
           " ~ ",
           group)
  )
  
  # # Outcome data
  # out_df <- as.numeric(data[[outcome]])
  # grp_df <- as.character(data[[group]])
  # 
  # # Get group standard deviations
  # grp_sd <- tapply(out_df,
  #                  grp_df,
  #                  sd,
  #                  na.rm = T)
  # 
  # # Get group means
  # grp_m <- tapply(out_df,
  #                 grp_df,
  #                 mean,
  #                 na.rm = T)
  # 
  # # Get group n
  # grp_n <- tapply(out_df,
  #                 grp_df,
  #                 \(x) length(na.omit(x)))
  
  
  
  # Run t-test
  t_test <- t.test(
    
    formula = formula,
    data = data,
    alternative = alternative,
    var.equal = var_equal, # If FALSE, then Welch's t-test
    conf.level = conf_level
    
  )
  

  # GET MODEL PARAMETERS ----------------------------------------------------
  
  # Get degrees of freedom
  df   <- t_test[["parameter"]][[1]]
  
  # Get mean difference
  m    <- t_test[["estimate"]][[1]] - t_test[["estimate"]][[2]]
  
  # Get test statistic
  stat <- t_test[["statistic"]][[1]]
  
  # Get standard error
  se <- m / stat
  
  # # Get pooled SD
  # sd_pooled <- sqrt(
  #   
  #   (grp_sd[[1]]^2 + grp_sd[[2]]^2) / 2
  #   
  # )
  
  # Get mean difference confidence interval
  m_ci <- t_test[["conf.int"]]
  
  m_ci_lower <- m_ci[1]
  m_ci_upper <- m_ci[2]
  
  
  # Calculate effect size
  es_df <- calc_es_d(
    
    outcome = data[[outcome]],
    group = data[[group]],
    # sd_pooled = sd_pooled,
    es_type = es_type,
    g_correction = g_correction,
    conf_level = conf_level
    
  )
  
  
  # Compile into t-test data frame
  tt_df <- data.frame(
    
    test = case_when(
      
      isTRUE(var_equal) ~ "Student",
      isFALSE(var_equal) ~ "Welch"
      
    ),
    stat = stat,
    df = df,
    p = t_test[["p.value"]],
    m_dif = m,
    se_dif = se,
    m_ci_lower = m_ci_lower,
    m_ci_upper = m_ci_upper,
    es_type = es_df[["es_type"]],
    es = es_df[["es"]],
    es_se = es_df[["es_se"]],
    es_ci_lower = es_df[["es_ci_lower"]],
    es_ci_upper = es_df[["es_ci_upper"]]
    
  )
  
  # Return
  return(tt_df)
  
}
