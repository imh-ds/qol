
#' Calculate Effect Size Cohen's d
#'
#' @description Calculate the Cohen's d, Hedges' g, or Glass's delta effect size
#'   for independent samples t-tests.
#'
#' @param outcome A vector of the outcome values.
#' @param group A vector of the grouping values.
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
#' @param paired A logical reflecting whether the pairwise comparison is between
#'   (FALSE) or within (TRUE) subjects. If within, then the pair is tested
#'   whether it is truly within by iterating through the unique between subjects
#'   combinations. \code{bs_unique} must be specified.
#' @param bs_unique A vector of characters indicating the unique levels of the
#'   between subjects variables.
#' @param within_vars A vector of characters indicating the repeated measures
#'   variables.
#'
#' @export
calc_es_d <- function(
  
  outcome,
  group,
  sd_pooled = NULL,
  es_type = "hedges",
  g_correction = "standard",
  conf_level = 0.95,
  paired = FALSE,
  bs_unique = NULL,
  within_vars = NULL
  
) {
  
  # Outcome data
  out_df <- as.numeric(outcome)
  grp_df <- as.character(group)
  
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
  
  # -- TRUE PAIRED FLAG -- #
  
  if (isTRUE(paired)) {
    
    # Identify possible groups
    possible_grps <- unique(
      purrr::reduce(
        str_split(grp_df,
                  " "),
        c
      )
    )
    
    # If all within_vars are in the list of possible groups, but each repeated
    # measure has unique between subjects, then flag as actually running
    # between subjects.
    bs_flag <- lapply(
      seq(ncol(bs_unique)),
      function(u) {
        
        bs_pair <- sum(unique(bs_unique[[u]]) %in% possible_grps) == 1
        
        return(bs_pair)
      }
    )
    
    # Get bs_paired logic for updating paired flag
    bs_paired <- any(unlist(bs_flag))
    
    # If all of within_vars aren't in the list of possible groups, then flag as
    # actually running between subjects since it is within a single measure and
    # not across repeated measures.
    paired <- all(within_vars %in% possible_grps) && bs_paired
    
  }
  
  

  # IF INDEPENDENT SAMPLES --------------------------------------------------

  if (isFALSE(paired)) {
    
    # Get means, standard deviations, and n
    g1_m <- grp_m[[1]]
    g2_m <- grp_m[[2]]
    g1_sd <- grp_sd[[1]]
    g2_sd <- grp_sd[[2]]
    g1_n <- grp_n[[1]]
    g2_n <- grp_n[[2]]
    
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
    
  }
  
  

  # IF PAIRED SAMPLES -------------------------------------------------------

  if (isTRUE(paired)) {
    
    pc_df <- tapply(
      out_df,
      grp_df,
      function(x) x
    )
    
    c1 <- pc_df[[1]]
    c2 <- pc_df[[2]]
    
    # Calculate differences between outcomes
    differences <- c1 - c2
    
    # Get sample size
    n <- length(differences)
    
    # Get alpha
    alpha <- 1 - conf_level
    
    
    # -- IF d OR g -- #
    
    # If Cohen's d or Hedges' g, get effect size
    if (es_type == "cohen" | es_type == "hedges") {
      
      
      # Calculate Cohen's d
      es <- mean(differences) / sd(differences)
      
      
      # If Cohen's d
      if (es_type == "cohen") {
        
        # From Line 254 from JASP ttestpairedsamples.R
        # https://github.com/jasp-stats/jaspTTests/blob/master/R/ttestpairedsamples.R#L254-L257
        
        # Calculate variance of Cohen's d
        es_var <- ((1/n) + (as.numeric(es)^2 / (2*n))) * (2*(1-cor(c1,c2)))
        
        # Calculate standard error of Cohen's d
        es_se <- sqrt(es_var)
        
        # Calculate confidence interval of Cohen's d
        es_ci_d <- psych::cohen.d.ci(
          es,
          n1 = n,
          alpha = alpha
        )

        es_ci_lower <- es_ci_d[1]
        es_ci_upper <- es_ci_d[3]
        
      }
      
      
      # If Hedges' g
      if (es_type == "hedges") {
        
        # Hedges' g correction
        correction_factor <- 1 - (3 / (4*(n-1) - 1))
        
        # Calculate Hedges' g
        es <- es * correction_factor
        
        # Calculate variance of Hedges' g
        es_var <- ((n - 1) + es^2) / (2 * n)
        
        # Calculate standard error of Hedges' g
        es_se <- sqrt(es_var)
        
        # Calculate confidence interval of Hedges' g
        t_value <- qt(1 - alpha/2,
                      df = n - 1)
        es_ci_lower <- es - t_value * es_se
        es_ci_upper <- es + t_value * es_se
        
      }
      
      
    }
    
    # Compile
    es_df <- data.frame(
      
      es_type = case_when(
        
        es_type == "cohen" ~ "Cohen's d",
        es_type == "hedges" ~ "Hedges' g",
        es_type == "glass" ~ "Glass's \u0394"
        
      ),
      es = es,
      es_se = es_se,
      es_ci_lower = es_ci_lower,
      es_ci_upper = es_ci_upper
      
    )
    
  }
  
  
  # Return
  return(es_df)
  
}
