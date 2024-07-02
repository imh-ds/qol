#' Calculate ANOVA Assumptions Tests
#'
#' @param data A dataframe object. This should be a structured dataset where
#'   each column represents a variable and each row represents an observation.
#' @param outcome Character indicating the outcome/dependent variable.
#' @param groups Character indicating the factor grouping variable.
#' @param aov_result The ANOVA model specified via \code{stats::aov()}.
#' 
#' @export
calc_aov_assumptions <- function(
    
  data,
  outcome,
  groups,
  aov_result
  
) {
  
  # -- NORMALITY ASSUMPTION TEST -- #
  
  # Grab data with no NA
  nt_var <- data[complete.cases(data[[outcome]]),] %>% 
    pull(outcome)
  
  # Grab residuals (errors) of ANOVA
  nt_res <- residuals(aov_result)
  
  
  # Shapiro-Wilk Normality Test
  sw_var <- shapiro.test(nt_var)
  sw_res <- shapiro.test(nt_res)
  
  # Kolmogorov-Smirnov Test Generally, Shapiro-Wilk and Anderson-Darling
  # preferred for behavioral science research due to KS assuming all values in
  # the data are unique. In BeSci research, the use of self-report survey
  # measures often mean a lot of tied values (i.e., duplicates). Thus, the high
  # degree of tied values will likely affect the shape of the empirical
  # distribution function (EDF) of the sample when compared against the
  # cumulative distribution function (CDF) of the specified theoretical
  # distribution, such as a normal distribution. The p-values may therefore be
  # inaccurate.
  ks_var <- suppressWarnings(
    ks.test(nt_var,
            "pnorm",
            mean = mean(nt_var),
            sd = sd(nt_var),
            alternative = "two.sided")
  )
  ks_res <- suppressWarnings(
    ks.test(nt_res,
            "pnorm",
            mean = mean(nt_res),
            sd = sd(nt_res))
  )
  
  # Anderson-Darling Test
  ad_var <- nortest::ad.test(nt_var)
  ad_res <- nortest::ad.test(nt_res)
  
  
  # Compile Normality-Test Assumptions
  normal_test <- data.frame(
    type = c("Variable", rep(NA, 2),
             "Residual Errors", rep(NA, 2)),
    method = rep(c("Shapiro-Wilk",
                   "Kolmogorov-Smirnov",
                   "Anderson-Darling"), 2),
    test = rep(c("W",
                 "D",
                 "A"), 2),
    statistic = c(sw_var[["statistic"]],
                  ks_var[["statistic"]],
                  ad_var[["statistic"]],
                  sw_res[["statistic"]],
                  ks_res[["statistic"]],
                  ad_res[["statistic"]]),
    p = c(sw_var[["p.value"]],
          ks_var[["p.value"]],
          ad_var[["p.value"]],
          sw_res[["p.value"]],
          ks_res[["p.value"]],
          ad_res[["p.value"]])
  )
  
  
  
  # -- HOMOGENEITY OF VARIANCE TESTS -- #
  
  # Bartlett Test
  bt_formula <- as.formula(paste0(outcome,
                                  " ~ interaction(",
                                  paste0(groups,
                                         collapse = ", "),
                                  ")"))
  bt <- bartlett.test(formula = bt_formula,
                      data = data)
  
  
  # Levene Test
  lt_formula <- as.formula(paste0(outcome,
                                  " ~ ",
                                  paste0(groups,
                                         collapse = "*")))
  lt <- suppressWarnings(
    car::leveneTest(y = lt_formula,
                        data = data,
                        center = "mean")
  )
  
  # Compile Homogeneity of Variance Test Assumptions
  hvar_test <- data.frame(
    
    method = c("Levene's",
               "Bartlett's"),
    test = c("F",
             "\u03C7\u00B2"),
    statistic = c(lt[["F value"]][[1]],
                  bt[["statistic"]][[1]]),
    df1 = c(lt[["Df"]][[1]],
            bt[["parameter"]][[1]]),
    df2 = c(lt[["Df"]][[2]],
            NA),
    p = c(lt[["Pr(>F)"]][[1]],
          bt[["p.value"]])
    
  )
  
  
  # Assumptions list
  assumption_list <- list(
    normality = normal_test,
    variance = hvar_test
  )
  
  # Return
  return(assumption_list)
  
  
}