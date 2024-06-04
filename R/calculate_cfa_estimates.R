#' Calculate Confirmatory Factor Analysis Estimates
#'
#' @description Calculate the latent variable estimates, reliability, and
#'   validity.
#'
#' @param u A character string of the latent variable name.
#' @param estimates The estimates data frame of a confirmatory factor analysis
#'   (CFA) \code{lavaan} summary object.
#'
#' @return A data frame of the latent variable estimates, reliability, and
#'   validity.
#'   
#' @export
calculate_cfa_est <- function(u,
                              estimates){
  
  # Get loadings from latent variable
  latent_loadings <- estimates %>% 
    dplyr::filter(lhs == u) %>% 
    dplyr::mutate(lhs = replace(lhs, duplicated(lhs), NA))
  
  # Get numeric loadings as vector
  loadings <- latent_loadings %>% 
    dplyr::pull(est) %>% 
    as.numeric()
  
  # Get loadings squared
  loadings_squared <- loadings^2
  
  # Get errors
  errors <- 1 - loadings_squared
  
  # Calculate Average Variance Extracted (AVE)
  ave <- sum(loadings_squared) / (sum(loadings_squared) + sum(errors))
  
  # Calculate Composite Reliability
  rhoc <- sum(loadings)^2 / (sum(loadings)^2 + sum(errors))
  
  # Get length of latent variable loadings
  length <- nrow(latent_loadings) - 1
  
  # Compile composite reliability and AVE into working dataframe
  metrics <- data.frame(rhoc = c(rhoc, rep(NA, length)),
                        ave = c(ave, rep(NA, length)))
  
  # Compile together into full table for the latent variable
  latent_variable_metrics <- cbind(latent_loadings,
                                   metrics)
  
  # Return the metrics for the latent variable
  return(latent_variable_metrics)
  
}
