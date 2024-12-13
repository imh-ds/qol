#' Generate PLS-SEM Model Pathways Text
#'
#' @param plssem_mod_effects 
#' @param digits 
#'
generate_plssem_pathtext <- function(
    plssem_mod_effects,
    digits = 3,
    latex = FALSE
) {
  
  # Set parameter for rounding
  rnd <- paste0('%.',
                digits,
                "f")
  
  # Generate texts
  text_effects <- plssem_mod_effects |> 
    
    # Generate texts
    dplyr::mutate(
      in_text = paste(if(isTRUE(latex)){"($\\beta$ = "} else {"(\u03B2 = "},
                      sprintf(rnd,
                              boot_est),
                      ", SE = ", base::sprintf(rnd, boot_se),
                      if(isTRUE(latex)){", *z* = "} else {", z = "},
                      sprintf(rnd,z),
                      ", 95% CI [", sprintf(rnd,lower_ci),
                      ", ", sprintf(rnd,upper_ci),
                      if(isTRUE(latex)){"], *p* "} else {"], p "}, 
                      ifelse(p < 0.001, "< 0.001", paste("=", sprintf(rnd,p))),")",
                      sep = ""),
      fig_text = paste(sprintf(rnd, boot_est),
                       dplyr::case_when(
                         
                         p > .05 ~ "",
                         p < .05 & p > .01 ~ "*",
                         p < .01 & p > .001 ~ "**",
                         p < .001 ~ "***"
                         
                       ),
                       " [", sprintf(rnd,lower_ci),
                       ", ", sprintf(rnd,upper_ci),
                       "]",
                       sep = "")
    )
  
  # Return
  return(text_effects)
  
}