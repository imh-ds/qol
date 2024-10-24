#' Generate Long Path Table for PLS-SEM
#'
#' @param wb 
#' @param plssem_mod 
#' @param digits 
#'
generate_plssem_table_long <- function(
  plssem_mod,
  digits = 3
) {
  
  # Set parameter for rounding
  rnd <- paste0('%.',
                digits,
                "f")
  
  paths_tab <- plssem_mod[["Direct_Effects"]]
  paths <- plssem_mod[["meta_data"]][["paths"]]
  pls_model <- plssem_mod[["PLS_Model"]]
  
  # Loop for every endogenous variables (e.g., mediators,)
  reg_list_long <- base::lapply(
    
    paths,
    function(var) {
      
      pt_outcome <- paths_tab %>%
        dplyr::filter(grepl(paste0(var,"$"),
                            path))
      
      pt_est <- pt_outcome %>% 
        dplyr::mutate(std_est = paste(sprintf(rnd, boot_est),
                                      case_when(
                                        
                                        p > .05 ~ "",
                                        p < .05 & p > .01 ~ "*",
                                        p < .01 & p > .001 ~ "**",
                                        p < .001 ~ "***"
                                        
                                      ))) %>% 
        dplyr::select(path, std_est)
      
      pt_ci <- pt_outcome %>% 
        dplyr::mutate(ci = paste0("(",
                                  sprintf(rnd, lower_ci),
                                  ", ",
                                  sprintf(rnd, upper_ci),
                                  ")")) %>% 
        dplyr::select(path,
                      ci)
      
      # Get degrees of freedom for number of predictors (numerator)
      degfree1 <- length(pt_est[["path"]])
      
      # Get degrees of freedom for number of observations minus predictors - 1 (denominator)
      degfree2 <- nrow(pls_model[["construct_scores"]]) - (1 + degfree1)
      
      pt_df1 <- data.frame("est" = c(rbind(pt_est[["std_est"]],
                                           pt_ci[["ci"]])))
      pt_df2 <- data.frame("variable" = c(rbind(pt_est[["path"]],
                                                paste0(sub("(.*)  ->  .*", "\\1",
                                                           pt_est[["path"]]),"_CI")))) %>% 
        dplyr::mutate(variable = gsub("  ->.*", "", variable))
      pt_df3 <- data.frame("variable" = c("Observations",
                                          "R\u00B2",
                                          "R\u00B2 Adj"),
                           "est" = c(nrow(pls_model[["construct_scores"]]),
                                     sprintf(rnd,
                                             as.data.frame(pls_model[["rSquared"]])[[var]])))
      
      # Get R^2 value for F-statistic calculation
      rsqr <- as.data.frame(pls_model[["rSquared"]])[[var]][1]
      
      # Calculate F-statistic
      pt_df4 <- data.frame("variable" = c("F"),
                           "est" = sprintf(rnd, 
                                           (rsqr / (1 - rsqr)) * (degfree2 / degfree1)))
      
      pt_table <- pt_df2 %>%
        cbind(pt_df1) %>% 
        rbind(pt_df3) %>% 
        rbind(pt_df4) %>% 
        dplyr::rename(!!sym(var) := est)
      
      return(pt_table)
      
    }
    
  )
  
  # Reduce
  reg_tab_long <- base::suppressMessages(
    
    purrr::reduce(reg_list_long,
                  dplyr::full_join)
    
  )
  
  # Collapse the path table list into one dataframe and send R^2 to the bottom
  reg_tab_longeff <- reg_tab_long %>%
    dplyr::filter(
      
      !base::grepl("R\\\u00B2|^Observations$|^F$",
                   variable)
      
    )
  
  reg_tab_longdesc <- reg_tab_long %>%
    dplyr::filter(
      
      base::grepl("R\\\u00B2|^Observations$|^F$",
                  variable)
      
    )
  
  # Combine into final product
  reg_long_table <- rbind(reg_tab_longeff,
                          reg_tab_longdesc) %>% 
    dplyr::mutate(variable = ifelse(grepl("_CI", variable), NA, variable)) %>% 
    dplyr::mutate(variable = gsub("\\*", " \u00D7 ", variable))
  
  # Return
  return(reg_long_table)
  
}
