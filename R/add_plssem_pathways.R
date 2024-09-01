#' Affix PLS-SEM Model Pathways Workbook Sheet
#'
#' @param wb 
#' @param plssem_mod 
#' @param sheet_name 
#' @param digits 
#'
#' @export
add_plssem_pathways <- function(
  wb,
  plssem_mod,
  sheet_name,
  name = NULL,
  digits = 3
) {
  
  # Set parameter for rounding
  rnd <- paste0('%.',
                digits,
                "f")
  
  # Detect whether indirect effects are in the PLS-SEM
  # If so, combine into one pathway model. Otherwise, only extract direct effects.
  if (isTRUE(plssem_mod[["meta_data"]][["indirect_effects"]])) {
    
    # Get both direct & indirect pathway effects
    direct_effs <- plssem_mod[["Direct_Effects"]] %>% 
      
      # Generate texts
      dplyr::mutate(
        in_text = paste("(\u03B2 = ",
                        sprintf(rnd,
                                boot_est),
                        ", SE = ", base::sprintf(rnd, boot_se),
                        ", z = ", sprintf(rnd,z),
                        ", 95% CI [", sprintf(rnd,lower_ci),
                        ", ", sprintf(rnd,upper_ci),
                        "], p ", ifelse(p < 0.001, "< 0.001", paste("=", sprintf(rnd,p))),")",
                        sep = ""),
        fig_text = paste(sprintf(rnd, boot_est),
                         case_when(
                           
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
      
    indirect_effs <- plssem_mod[["Indirect_Effects"]] %>% 
      
      # Generate texts
      dplyr::mutate(
        in_text = paste("(\u03B2 = ",
                        sprintf(rnd,
                                boot_est),
                        ", SE = ", base::sprintf(rnd, boot_se),
                        ", z = ", sprintf(rnd,z),
                        ", 95% CI [", sprintf(rnd,lower_ci),
                        ", ", sprintf(rnd,upper_ci),
                        "], p ", ifelse(p < 0.001, "< 0.001", paste("=", sprintf(rnd, p))),")",
                        sep = ""),
        fig_text = paste(sprintf(rnd, boot_est),
                         dplyr::case_when(
                           
                           p > .05 ~ "",
                           p < .05 & p > .01 ~ "*",
                           p < .01 & p > .001 ~ "**",
                           p < .001 ~ "***"
                           
                         ),
                         " [", sprintf(rnd, lower_ci),
                         ", ", sprintf(rnd, upper_ci),
                         "]",
                         sep = "")
      )
    
    # Get nrow lengths for formatting later
    de_row <- nrow(direct_effs)
    ie_row <- nrow(indirect_effs)
    
    # Grab column names
    eff_cols <- colnames(direct_effs)
    
    # Create effect type labels
    eff_types <- data.frame(
      matrix(
        ncol = length(eff_cols),
        nrow = 2
      )
    )
    
    # Rename the effect type columns
    colnames(eff_types) <- eff_cols
    
    # Fill in effect types
    eff_types[1, 1] <- "Direct Effects"
    eff_types[2, 1] <- "Indirect Effects"
    
    # Remove all other information
    eff_types[1,-1] <- NA
    eff_types[2,-1] <- NA
    
    # Create effects dataframe
    effects <- rbind(
      eff_types[1,],
      direct_effs,
      NA,
      eff_types[2,],
      indirect_effs
    )
    
  } else {
    
    # Get direct pathway effects
    effects <- plssem_mod[["Direct_Effects"]] %>% 
      
      # Generate texts
      dplyr::mutate(
        in_text = paste("(\u03B2 = ",
                        sprintf(rnd,
                                boot_est),
                        ", SE = ", base::sprintf(rnd, boot_se),
                        ", z = ", sprintf(rnd, z),
                        ", 95% CI [", sprintf(rnd, lower_ci),
                        ", ", sprintf(rnd, upper_ci),
                        "], p ", ifelse(p < 0.001, "< 0.001", paste("=", sprintf(rnd, p))),")",
                        sep = ""),
        fig_text = paste(sprintf(rnd, boot_est),
                         dplyr::case_when(
                           
                           p > .05 ~ "",
                           p < .05 & p > .01 ~ "*",
                           p < .01 & p > .001 ~ "**",
                           p < .001 ~ "***"
                           
                         ),
                         " [", sprintf(rnd, lower_ci),
                         ", ", sprintf(rnd, upper_ci),
                         "]",
                         sep = "")
      )
    
  }
  
  # Recode the arrows
  effects <- effects %>% 
    
    # The default seminr pathway arrows include 2 spaces, reduce to one
    dplyr::mutate(
      path = gsub("  ",
                  " ",
                  path)
    ) %>% 
    
    # Change the "->" to unicode right arrow
    dplyr::mutate(
      path = gsub("->",
                  "\u2192",
                  path)
    )
  
  
  
  # Create renamer
  rename_mod <- function(
    model) {
    
    model_num <- model %>% 
      
      # Select specific vars
      dplyr::select(
        -c(est,
           in_text,
           fig_text)
      ) %>% 
      
      # Rename vars
      dplyr::rename(
        Pathway = path,
        "\u03B2\u1D47" = boot_est,
        "SE\u1D47" = boot_se,
        Lower = lower_ci,
        Upper = upper_ci
      )
    
    model_txt <- model %>% 
      
      # Select specific vars
      dplyr::select(
        in_text,
        fig_text
      ) %>% 
      
      # Rename vars
      dplyr::rename(
        "In Text" = in_text,
        "Fig Text" = fig_text
      )
    
    mod_list <- list(
      model_num,
      model_txt
    )
    
    return(mod_list)
    
  }
  
  
  # Grab data frames
  model <- rename_mod(effects)[[1]]
  model_txt <- rename_mod(effects)[[2]]

  
  # Define rows and columns
  mod_row <- nrow(model)
  mod_col <- ncol(model)
  
  mod_t_row <- nrow(model_txt)
  mod_t_col <- ncol(model_txt)
  
  
  # Add model performance worksheet
  openxlsx::addWorksheet(wb,
                         sheet_name)
  
  # Starting column
  start_col = 2
  
  # Starting row
  start_row = 3
  
  
  
  # TITLE -------------------------------------------------------------------
  
  openxlsx::writeData(
    wb = wb,
    sheet = sheet_name,
    x = if(!is.null(name)) {
      paste0(name,
             " - PLS-SEM Pathways Table")
    } else {
      "PLS-SEM Pathways Table"
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
  
  

  # WRITE TABLE -------------------------------------------------------------
  
  openxlsx::writeData(
    wb,
    sheet = sheet_name,
    x = "Pathway Table",
    startCol = start_col,
    startRow = start_row + 2
  )
  openxlsx::mergeCells(
    wb,
    sheet = sheet_name,
    cols = 6:7,
    rows = start_row + 3
  )
  openxlsx::writeData(
    wb,
    sheet = sheet_name,
    x = "95% CI",
    startCol = 6,
    startRow = start_row + 3
  )
  openxlsx::writeData(
    wb,
    sheet = sheet_name,
    x = model,
    startCol = start_col,
    startRow = start_row + 4
  )
  openxlsx::writeData(
    wb,
    sheet = sheet_name,
    x = "NOTE: \u1D47 bootstrapped values.",
    startCol = start_col,
    startRow = start_row + 4 + mod_row + 1
  )
  
  # Apply formatter
  apply_plssem_path_formatter(
    wb = wb,
    sheet = sheet_name,
    df = model,
    start_col = start_col,
    start_row = start_row + 2,
    digits = digits,
    direct_nrow = if(isTRUE(plssem_mod[["meta_data"]][["indirect_effects"]])) {de_row} else {NULL},
    indirect_nrow = if(isTRUE(plssem_mod[["meta_data"]][["indirect_effects"]])) {ie_row} else {NULL}
  )
  
  
  openxlsx::writeData(
    wb,
    sheet = sheet_name,
    x = "Pathway Text",
    startCol = (start_col + 1 + mod_col),
    startRow = start_row + 2
  )
  openxlsx::writeData(
    wb,
    sheet = sheet_name,
    x = model_txt,
    startCol = (start_col + 1 + mod_col),
    startRow = (start_row + 4)
  )
  
  # Apply formatter
  apply_text_format(
    wb = wb,
    sheet = sheet_name,
    df = model_txt,
    start_col = (start_col + 1 + mod_col),
    start_row = start_row + 2,
    digits = digits
  )
  
  
  # Expand column width of col B
  openxlsx::setColWidths(
    wb,
    sheet = sheet_name,
    cols = "B",
    widths = 30
  )
  
  # Hide gridlines
  openxlsx::showGridLines(
    wb,
    sheet = sheet_name,
    showGridLines = FALSE
  )
  
}
