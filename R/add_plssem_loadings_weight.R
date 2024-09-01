#' Add PLS-SEM Model Loadings & Weight Workbook Sheet
#' 
#' @description
#' a
#' 
#' 
#' @param wb 
#' @param plssem_mod 
#' @param sheet_name 
#' @param digits 
#'
#' @export
add_plssem_load_wgt <- function(
  wb,
  plssem_mod,
  sheet_name,
  name = NULL,
  digits = 3
) {
  
  # Get Pathway Table
  model_m <- plssem_mod[[sheet_name]] %>% 
    dplyr::rename(
      
      Path = "path",
      "\u03B2" = est,
      "\u03B2\u1D47" = boot_est,
      "SE\u1D47" = boot_se,
      "Lower" = lower_ci,
      "Upper" = upper_ci
      
    ) %>% 
    dplyr::select(-t)
  
  
  # Get data frames
  if(sheet_name == "Loadings") {
    
    # Get Loadings Table
    model_t <- plssem_mod[["Loadings_Table"]] %>% 
      dplyr::rename(
        Variable = variable
      )
    
    title_table <- "Loadings Table"
    title_metrics <- "Loadings by Pathway"
    
  }
  
  if(sheet_name == "Weights") {
    
    # Get Weights Table
    model_t <- plssem_mod[["Weights_Table"]] %>% 
      dplyr::rename(
        Variable = variable
      )
    
    title_table <- "Weights Table"
    title_metrics <- "Weights by Pathway"
    
  }
  
  
  # Define rows and columns
  mod_t_row <- nrow(model_t)
  mod_t_col <- ncol(model_t)
  
  mod_m_row <- nrow(model_m)
  mod_m_col <- ncol(model_m)
  
  
  # Add model performance worksheet
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
             " - PLS-SEM ",
             sheet_name)
    } else {
      paste0("PLS-SEM ",
             sheet_name)
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
  
  

  # WRITE MODEL LOADINGS & WEIGHTS ------------------------------------------

  # -- WRITE TABLE -- #
  
  openxlsx::writeData(
    wb,
    sheet = sheet_name,
    x = title_table,
    startCol = start_col,
    startRow = start_row + 2
  )
  openxlsx::writeData(
    wb,
    sheet = sheet_name,
    x = model_t,
    startCol = start_col,
    startRow = start_row + 3
  )
  
  
  # Apply formatter
  apply_plssem_perf_formatter(
    wb = wb,
    sheet = sheet_name,
    df = model_t,
    start_col = start_col,
    start_row = start_row + 2,
    digits = digits
  )
  
  
  
  # -- WRITE METRICS TABLE -- #
  
  openxlsx::writeData(
    wb,
    sheet = sheet_name,
    x = title_metrics,
    startCol = start_col,
    startRow = (start_row + 6 + mod_t_row)
  )
  openxlsx::mergeCells(
    wb,
    sheet = sheet_name,
    cols = 7:8,
    rows = (start_row + 7 + mod_t_row)
  )
  openxlsx::writeData(
    wb,
    sheet = sheet_name,
    x = "95% CI",
    startCol = 7,
    startRow = (start_row + 7 + mod_t_row)
  )
  openxlsx::writeData(
    wb,
    sheet = sheet_name,
    x = model_m,
    startCol = start_col,
    startRow = (start_row + 8 + mod_t_row)
  )
  openxlsx::writeData(
    wb,
    sheet = sheet_name,
    x = "NOTE: \u1D47 bootstrapped values.",
    startCol = start_col,
    startRow = (start_row + 8 + mod_t_row + mod_m_row + 1)
  )
  
  # Apply formatter
  apply_plssem_path_formatter(
    
    wb = wb,
    sheet = sheet_name,
    df = model_m,
    start_col = start_col,
    start_row = (start_row + 6 + mod_t_row),
    ci_col = 7:8,
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