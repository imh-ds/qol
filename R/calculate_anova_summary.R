#' Calculate ANOVA Summary
#'
#' @param aov_model The ANOVA model specified via \code{stats::aov()}.
#'
#' @export
calc_aov_summary <- function(
  
  aov_model
  
) {
  
  # Effect size for ANOVA
  aov_summary <- tibble::as_tibble(
    sjstats::anova_stats(
      aov_model,
      digits = 22
    )
  ) %>% 
    
    # Rename variables
    dplyr::rename(
      
      effect = term,
      f_value = statistic,
      p_value = p.value,
      etasq_partial = partial.etasq,
      omegasq_partial = partial.omegasq,
      cohens_f = cohens.f
      
    ) %>% 
    
    # Reorder
    dplyr::select(
      
      effect,
      df,
      sumsq,
      meansq,
      f_value,
      p_value,
      dplyr::everything()
      
    ) %>% 
    
    # Recode ":" into "*"
    dplyr::mutate(
      
      effect = gsub(":",
                    "*",
                    effect)
      
    )
  
  # Return
  return(aov_summary)
  
}
