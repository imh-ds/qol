#' Calculate ANOVA Overall Model
#'
#' @param aov_summary ANOVA summary dataframe created from
#'   \code{calc_aov_summary()}.
#'
#' @export
calc_aov_overall <- function(
    
  aov_summary = .
  
) {
  
  # Get residuals
  residual_stats <- aov_summary %>% 
    
    # Filter to just the Residuals row
    dplyr::filter(effect == "Residuals") %>% 
    
    # Grab just the df and MeanSq values
    dplyr::summarize(df = df,
                     meansq = meansq) %>% 
    
    # Pull
    base::unlist()
  
  # Compute overall model
  overall_model <- aov_summary %>% 
    
    # Filter to everything but the Residuals
    dplyr::filter(effect != "Residuals") %>% 
    
    # Select the df and SumSq
    dplyr::select(df, sumsq) %>% 
    
    # Summarize
    dplyr::summarise(dplyr::across(.cols = dplyr::everything(),
                                   \(x) sum(x, na.rm = T))) %>% 
    
    # Mutate new values
    dplyr::mutate(effect = "Overall Model",
                  meansq = sumsq / df,
                  f_value = meansq / residual_stats[["meansq"]],
                  p_value = 1 - pf(f_value, df, residual_stats[["df"]])) %>% 
    
    # Relocate the effect to the first column
    dplyr::relocate(effect,
                    .before = df)
  
  # Bind rows of the ANOVA summary with the Overall Model statistics
  aov_summary <- overall_model %>% 
    dplyr::bind_rows(aov_summary)
  
  # Return
  return(aov_summary)
  
}
