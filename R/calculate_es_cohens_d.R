
#' Calculate Effect Size Cohen's d
#'
#' @description Calculate the Cohen's d, Hedges' g, or Glass's delta effect size
#' for independent samples t-tests.
#'
#' @param g1_m Numeric value of group 1's mean.
#' @param g2_m Numeric value of group 2's mean.
#' @param g1_sd Numeric value of group 1's standard deviation.
#' @param g2_sd Numeric value of group 2's standard deviation.
#' @param g1_n Numeric value of group 1's sample size n.
#' @param g2_n Numeric value of group 2's sample size n.
#' @param sd_pooled Optional numeric value of the pooled standard deviation. If
#'   not specified, the pooled standard deviation is calculated from group 1 and
#'   2's standard deviations.
#' @param es_type Character indicating which effect size to return. Options
#'   include \code{"cohen"} for Cohen's d, \code{"hedges"} for Hedges' g, or
#'   \code{"glass"} for Glass's delta. The default is Hedges' g.
#' @param g_correction Character indicating which Hedges' g correction to apply.
#'   Options include \code{"standard"} for regular Hedges' g correction and
#'   \code{"log"} to apply logarithmic Hedges' g correction. The default is
#'   standard.
#' @param conf_level Numeric value reflecting the confidence interval level. The
#'   default is 0.95 for 95% confidence interval.
#'
#' @return
#' 
#' @examples
#' 
#' @export
calc_es_d <- function(
    
  g1_m,
  g2_m,
  g1_sd,
  g2_sd,
  g1_n,
  g2_n,
  sd_pooled = NULL,
  es_type = "hedges",
  g_correction = "standard",
  conf_level = 0.95
  
) {
  
  # If pooled SD is not specified, calculate
  if (is.null(sd_pooled)) {
    
    # Get pooled SD
    sd_pooled <- sqrt(
      
      (g1_sd^2 + g2_sd^2) / 2
      
    )
    
  }
  
  
  # If Cohen's d or Hedges' g, get effect size
  if (es_type == "cohen" | es_type == "hedges") {
    
    # Calculate Cohen's d
    es <- (g1_m - g2_m) / sd_pooled
    
    # If Hedges' g correction
    if (es_type == "hedges") {
      
      # If applying Hedges' g standard correction
      if (g_correction == "standard") {
        
        correction_factor <- 1 - (3 / (4 * (g1_n + g2_n) - 9))
        
        es <- es * correction_factor
        
      }
      
      # If applying Hedges' g log correction
      if (g_correction == "log") {
        
        a <- sum(g1_n, g2_n) - 2
        
        # Run log correction
        correction_factor <- lgamma(a / 2) - (log(sqrt(a / 2)) + lgamma((a - 1) / 2))
        
        es <- es * exp(correction_factor)
        
      }
      
    }
    
  }
  
  
  # If Glass's Delta, get effect size
  if (es_type == "glass") {
    
    es <- as.numeric((g1_m - g2_m) / g2_sd)
    
  }
  
  
  # Compute effect size SE
  if (es_type == "hedges") {
    
    if (g_correction == "standard") {
      
      j <- correction_factor
      
    } 
    
    if (g_correction == "log") {
      
      j <- exp(correction_factor)
      
    }
    
  } else {
    
    j <- 1
    
  }
  
  # If Glass's delta, get n of intervention group
  n_int <- if (es_type == "glass") {
    
    n_int <- g2_n
    
  } else {
    
    # Otherwise, just get n of both groups
    n_int <- c(g1_n, g2_n)
    
  }
  
  
  # Calculate variance of effect size
  es_var <- j^2 * (sum(g1_n, g2_n)/prod(g1_n, g2_n) + (as.numeric(es)^2 / (2*sum(n_int))))
  
  # Introduction to Meta-Analysis. Michael Borenstein, L. V. Hedges, J. P. T.
  # Higgins and H. R. Rothstein (2009). Chapter 4, equation (4.20/4.24).
  # Calculate standard error of effect size
  es_se <- sqrt(es_var)
  
  # Get confidence interval of effect size
  es_ci <- MBESS::ci.smd(
    
    smd = es,
    n.1 = g1_n,
    n.2 = g2_n,
    conf.level = conf_level
    
  )
  
  # Compile
  es_df <- data.frame(
    
    es_type = case_when(
      
      es_type == "cohen" ~ "Cohen's d",
      es_type == "hedges" ~ "Hedges' g",
      es_type == "glass" ~ "Glass's \u0394"
      
    ),
    es = es_ci[["smd"]],
    es_se = es_se,
    es_ci_lower = es_ci[["Lower.Conf.Limit.smd"]],
    es_ci_upper = es_ci[["Upper.Conf.Limit.smd"]]
    
  )
  
  # Return
  return(es_df)
  
}
