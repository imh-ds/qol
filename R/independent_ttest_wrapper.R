#' Independent Samples T-Test Wrapper
#'
#' @description A wrapper function to automate running and extracting the
#'   results of independent samples t-tests. The current function is limited to
#'   running Student's, Welch's, and Wilcoxon/Mann-Whitney U tests. Binary group
#'   effect sizes are limited to Cohen's d, Hedges' g, and Glass's delta.
#'
#' @param data A dataframe object. This should be a structured dataset where
#'   each column represents a variable and each row represents an observation.
#' @param outcomes A vector of characters indicating the outcome/dependent
#'   variables.
#' @param group Character indicating the binary factor grouping variable.
#' @param test A vector of characters indicating which independent samples test
#'   to run. Options include \code{"student"} for Student's t-test,
#'   \code{"welch"} for Welch's t-test, and \code{"wilcox"} for
#'   Wilcoxon/Mann-Whitney U test. The default is to run all three.
#' @param alternative Character indicating the direction of the alternative
#'   hypothesis. Options include \code{"two.sided"}, \code{"greater"}, or
#'   \code{"less"}. The default is two sided alternative hypothesis.
#' @param conf_level Numeric value reflecting the confidence interval level. The
#'   default is 0.95 for 95% confidence interval.
#' @param digits Number of decimal places for the correlation matrix. Default is
#'   3 decimal points. If not specified, the function will use the default
#'   value.
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
wrap_ttest <- function(
    data = .,
    outcomes,
    group,
    test = c("student", "welch", "wilcox"),
    alternative = "two.sided",
    conf_level = 0.95,
    digits = 3,
    es_type = "hedges",
    g_correction = "standard") {
  
  
  
  # -- ERROR MESSAGES -- #
  
  # Stop if confidence interval level > 1
  if (conf_level > 1) {
    
    stop("Confidence Level cannot be greater than 1. Please change to within the range of 0 to 1.")
    
  }
  
  
  
  # -- SET BASIC PARAMETERS -- #
  
  # Set rounding parameter
  round <- paste0(
    '%.', digits, 'f'
  )
  
  
  # -- GET DESCRIPTIVE STATISTICS -- #
  
  desc_list <- lapply(
    outcomes,
    function(o) {
      
      # Outcome data
      out_df <- data[[o]]
      grp_df <- data[[group]]
      
      # Get group standard deviations
      grp_sd <- tapply(out_df,
                       grp_df,
                       sd,
                       na.rm = T)
      
      # Get group means
      grp_m <- tapply(out_df,
                      grp_df,
                      mean,
                      na.rm = T)
      
      # Get group n
      grp_n <- tapply(out_df,
                      grp_df,
                      \(x) length(na.omit(x)))
      
      # Compile into descriptive df
      desc_df <- data.frame(
        
        outcome = c(o, NA),
        group = names(grp_n),
        n = as.numeric(grp_n),
        mean = as.numeric(grp_m),
        sd = as.numeric(grp_sd)
        
      ) %>% 
        
        # Mutate new variables
        mutate(
          
          # Calculate standard error
          se = sd / sqrt(n),
          
          # Reportable text in format of M ± SD
          text_pm = paste0(
            "(M \u00B1 SD = ",
            sprintf(round, mean),
            " \u00B1 ",
            sprintf(round, sd),
            ")"
          ),
          
          # Reportable text in format of M, SD
          text_msd = paste0(
            "(M = ",
            sprintf(round, mean),
            ", SD = ",
            sprintf(round, sd),
            ")"
          )
          
        )
      
      # Return
      return(desc_df)
      
    }
  )
  
  # Reduce descriptive statistics
  desc_table <- purrr::reduce(desc_list,
                              rbind)
  
  
  # -- RUN ASSUMPTION CHECKS - SHAPIRO WILK NORMALITY TEST -- #
  
  normal_list <- lapply(
    outcomes,
    function(o) {
      
      # Working dataframe
      wrk_df <- data[c(o, group)]
      
      # Split dataframe
      df <- split(wrk_df, wrk_df[[group]])
      
      # Get names
      names <- names(df)
      
      # Group 1 dataframe
      g1_df <- df[[1]]
      
      # Group 2 dataframe
      g2_df <- df[[2]]
      
      # Shapiro-Wilk Test of Normality
      g1_sw <- shapiro.test(g1_df[[o]])
      g2_sw <- shapiro.test(g2_df[[o]])
      
      # Compile Shapiro-Wilk results
      sw_df <- data.frame(
        
        outcome = c(o, NA),
        group = names,
        W = c(g1_sw[["statistic"]],
              g2_sw[["statistic"]]),
        p = c(g1_sw[["p.value"]],
              g2_sw[["p.value"]])
        
      )
      
      # Return
      return(sw_df)
      
    }
  )
  
  # Reduce
  normal_table <- purrr::reduce(normal_list,
                                rbind)
  
  
  # -- RUN ASSUMPTION CHECKS - EQUALITY OF VARIANCE - LEVENE -- #
  
  equal_list <- lapply(
    outcomes,
    function(o) {
      
      # Set formula
      formula <- as.formula(
        paste0(o,
               " ~ as.factor(",
               group,
               ")")
      )
      
      # Run Brown-Forsythe test
      bf <- car::leveneTest(formula,
                            data = data,
                            center = "median")
      
      # Run Levene's test
      lt <- car::leveneTest(formula,
                            data = data,
                            center = "mean")
      
      # Compile results into equality dataframe
      eq_df <- data.frame(
        
        outcome = c(o, NA),
        test = c("Brown-Forsythe",
                 "Levene's"),
        center = c("Median",
                   "Mean"),
        "F" = c(bf[["F value"]][[1]],
                lt[["F value"]][[1]]),
        df_num = c(bf[["Df"]][[1]],
                   lt[["parameter"]][[1]]),
        df_den = c(bf[["Df"]][[2]],
                   lt[["parameter"]][[2]]),
        p = c(bf[["Pr(>F)"]][[1]],
              lt[["Pr(>F)"]][[1]])
        
      )
      
      # Return
      return(eq_df)
      
    }
  )
  
  # Reduce
  equal_table <- purrr::reduce(equal_list,
                               rbind)
  
  
  # -- RUN T-TEST -- #
  
  ttest_list <- lapply(
    outcomes,
    function(o) {
      
      # Initialize
      ttest_df <- NULL
      
      # If test includes Student t-test
      if (any(test == "student")) {
        
        # Run Student's t-test
        student <- calc_ind_ttest(
          
          outcome = o,
          group = group,
          data = data,
          alternative = alternative,
          var_equal = TRUE,
          conf_level = conf_level,
          es_type = es_type,
          g_correction = g_correction
          
        )
        
        # Add
        ttest_df <- rbind(ttest_df,
                          student)
        
      }
      
      
      # If test vector includes Welch's t-test
      if (any(test == "welch")) {
        
        # Run Welch's t-test
        welch <- calc_ind_ttest(
          
          outcome = o,
          group = group,
          data = data,
          alternative = alternative,
          var_equal = FALSE,
          conf_level = conf_level,
          es_type = es_type,
          g_correction = g_correction
          
        )
        
        # Add
        ttest_df <- rbind(ttest_df,
                          welch)
        
      }
      
      
      # Run Mann-Whitney u-test
      if (any(test == "wilcox")) {
        
        # Run Wilcoxon Rank-Sum test
        wilcox <- calc_wilcox_test(
          
          outcome = o,
          group = group,
          data = data,
          alternative = alternative,
          conf_level = conf_level
          
        )
        
        # Add
        ttest_df <- rbind(ttest_df,
                          wilcox)
        
      }
      
      
      # Write outcome name
      out_name <- data.frame(
        outcome = c(o, rep(NA, length(test) - 1))
      )
      
      # Combine
      ttest_df <- cbind(out_name,
                        ttest_df)
      
      # Return
      return(ttest_df)
      
    }
  )
  
  # Reduce
  ttest_table <- purrr::reduce(ttest_list,
                               rbind)
  
  
  # Compile into list to return
  return_list <- list(
    
    descriptive = desc_table,
    normality = normal_table,
    equality = equal_table,
    ttest = ttest_table
    
  )
  
  # Return
  return(return_list)
  
  
}
