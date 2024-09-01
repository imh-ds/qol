#' Title
#'
#' @param wb 
#' @param plssem_mod 
#' @param sheet_name 
#' @param digits 
#'
#' @export
add_plssem_long_table <- function(
    
  wb,
  plssem_mod,
  sheet_name,
  name = NULL,
  digits = 3  
  
) {
  
  # MODEL PARAMETERS --------------------------------------------------------
  
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
  
  

  # TABLE PARAMETERS --------------------------------------------------------

  # Get table
  table <- reg_long_table
  
  # Get path names
  path_names <- c("",
                  names(table)[2:ncol(table)])
  
  # Get model names
  mod_names <- c("",
                 sprintf("Model %01d",
                         seq(length(path_names)-1)))
  
  # Create header name dataframe
  header_names <- t(
    as.data.frame(
      path_names
    )
  ) %>% 
    magrittr::set_colnames(
      .,
      mod_names
    )
  
  # Rename table
  names(table) <-  c("Variable",
                     rep("\u03B2 (95% CI)",
                         length(path_names)-1))
  
  
  
  # Define rows and columns
  mod_row <- nrow(table)
  mod_col <- ncol(table)
  
  # Specify table title
  title_table <- "Direct Effects Table"
  
  # Add model worksheet
  openxlsx::addWorksheet(wb,
                         sheet_name)
  
  # Starting column
  start_col = 2
  
  # Starting row
  start_row = 3
  
  
  
  # TITLE -------------------------------------------------------------------
  
  # -- TITLE -- #
  openxlsx::writeData(
    wb = wb,
    sheet = sheet_name,
    x = if(!is.null(name)) {
      paste0(name,
             " - PLS-SEM Table (Long Version)")
    } else {
      "PLS-SEM Table (Long Version)"
    },
    startCol = start_col,
    startRow = start_row
  )
  
  # Apply title format
  openxlsx::addStyle(
    wb = wb,
    sheet = sheet_name,
    style = openxlsx::createStyle(
      fontSize = 20,
      textDecoration = "bold"
    ),
    cols = start_col,
    rows = start_row
  )
  
  
  
  # WRITE MODEL TABLE -------------------------------------------------------
  
  # -- WRITE TABLE -- #
  
  openxlsx::writeData(wb,
                      sheet = sheet_name,
                      x = title_table,
                      startCol = start_col,
                      startRow = start_row + 2)
  openxlsx::writeData(wb,
                      sheet = sheet_name,
                      x = header_names,
                      startCol = start_col,
                      startRow = start_row + 3)
  openxlsx::writeData(wb,
                      sheet = sheet_name,
                      x = "",
                      startCol = start_col,
                      startRow = start_row + 3)
  openxlsx::writeData(wb,
                      sheet = sheet_name,
                      x = table,
                      startCol = start_col,
                      startRow = start_row + 5)
  openxlsx::writeData(wb,
                      sheet = sheet_name,
                      x = "NOTE: All effects are bootstrapped. *** p < .001, ** p < .01, * p < .05.",
                      startCol = start_col,
                      startRow = start_row + 5 + mod_row + 1)
  
  # Apply formatter
  apply_plssem_long_table_formatter(
    wb = wb,
    sheet = sheet_name,
    df = table,
    start_col = start_col,
    start_row = start_row + 2,
    digits = digits
  )
  
  # Expand column width of col B
  openxlsx::setColWidths(wb,
                         sheet = sheet_name,
                         cols = "B",
                         widths = 30)
  
  # Expand column width of main body columns
  openxlsx::setColWidths(wb,
                         sheet = sheet_name,
                         cols = 3:(mod_col+1),
                         widths = 15)
  
  
  # Hide gridlines
  openxlsx::showGridLines(wb,
                          sheet = sheet_name,
                          showGridLines = FALSE)
  
}