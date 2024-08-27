#' Calculate Effect Size Mean Difference
#'
#' @param mean 
#' @param sd 
#' @param n 
#' @param sd_pooled 
#' @param r 
#' @param paired 
#' @param conf_level 
#' @param type 
#'
#' @export
calc_esm <- function(
  
  mean,
  sd = NULL,
  n,
  sd_pooled = NULL,
  r = NULL,
  paired = FALSE,
  conf_level = 0.95,
  type = "hedges"
  
) {
  

  # SPECIFY EFFECT SIZE PARAMETERS ------------------------------------------

  # Specify descriptive statistics
  m1 <- mean[1]
  m2 <- mean[2]
  
  if (!is.null(sd)) {
    sd1 <- sd[1]
    sd2 <- sd[2]
  }

  n1 <- n[1]
  n2 <- n[2]
  
  
  # Calculate pairwise statistics
  n_tilde <- psych::harmonic.mean(n)
  m_diff <- abs(m2 - m1)
  
  if (is.null(sd_pooled)) {
    
    sd_pooled <- sqrt(((n1-1)*sd1^2) + ((n2-1)*sd2^2)) / sqrt(n1+n2-2)
    
  }
  

  # CALCULATE EFFECT SIZE ---------------------------------------------------

  # If mean difference effect size is specified as Cohen's d or Hedges' g
  if (type %in% c("cohen", "hedges", "d", "g")) {
    
    # Compute Cohen’s d
    d <- m_diff / sd_pooled
    
    # If type specified as Cohen's d, finalize effect size as d
    if (type %in% c("cohen", "d")) {
      
      es <- d
      eta <- n1 + n2
      
    }
    
    # If type specified as Hedges' g, finalize effect size as g
    if (type %in% c("hedges", "g")) {
      
      # Compute Hedges’ g
      eta <- n1 + n2 - 2
      
      # Correction factor j
      j <- lgamma(eta / 2) - (log(sqrt(eta / 2)) + lgamma((eta - 1) / 2))
      g <- d * exp(j)
      
      es <- g
      
    } else {
      
      j <- 1
      
    }
    
  }
  
  
  # If type specified as Glass's delta
  if (type %in% c("glass", "delta")) {
    
    # Compute Glass's delta
    delta <- m_diff / sd2
    
    es <- delta
    eta <- n1 + n2
    
  }
  
  
  # If Glass's delta, get n of intervention group
  n_int <- if (type == "glass") {
    
    n2
    
  } else {
    
    # Otherwise, just get n of both groups
    c(n1, n2)
    
  }
  
  
  # Calculate variance of effect size
  es_var <- j^2 * (sum(n1, n2)/prod(n1, n2) + (as.numeric(es)^2 / (2*sum(n_int))))
  
  # Introduction to Meta-Analysis. Michael Borenstein, L. V. Hedges, J. P. T.
  # Higgins and H. R. Rothstein (2009). Chapter 4, equation (4.20/4.24).
  # Calculate standard error of effect size
  es_se <- sqrt(es_var)
  
  
  # Calculate non-centrality parameter
  lambda <- if (isFALSE(paired)) {
    
    es * sqrt(n_tilde/2)
    
  } else {
    
    es * sqrt(n_tilde/(2 * (1-r)))
    
  }
  
  # Calculate confidence interval of the effect size
  t_lower <- suppressWarnings(
    qt(1/2 - conf_level/2, 
       df = eta, 
       ncp = lambda)
  )
  t_upper <- suppressWarnings(
    qt(1/2 + conf_level/2, 
       df = eta, 
       ncp = lambda)
  )
  es_ci_lower <- t_lower / lambda * es
  es_ci_upper <- t_upper / lambda * es
  
  # Compile
  es_return <- data.frame(
    es_type = dplyr::case_when(
      type %in% c("hedges", "g") ~ "Hedges' g",
      type %in% c("cohen", "d") ~ "Cohen's d",
      type %in% c("glass", "delta") ~ "Glass's \u0394"
    ),
    es = es,
    es_se = es_se,
    es_ci_lower = es_ci_lower,
    es_ci_upper = es_ci_upper
  )
  
  # Return
  return(es_return)
  
}
