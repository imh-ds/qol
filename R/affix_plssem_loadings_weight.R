#' Affix PLS-SEM Model Loadings & Weight Workbook Sheet
#'
#' @param wb 
#' @param plssem_mod 
#' @param sheet_name 
#' @param digits 
#'
#' @return
#' @export
#'
#' @examples
affix_plssem_load_wgt <- function(
    wb,
    plssem_mod,
    sheet_name,
    digits = 3) {
  
  # Get data frames
  if(sheet_name == "Loadings") {
    
    # Get Loadings Table
    model_t <- plssem_mod[["Loadings_Table"]] %>% 
      magrittr::set_colnames(
        ., stringr::str_to_title(base::names(.))
      )
    
    # Get Loadings Pathway Table
    model_m <- plssem_mod[["Loadings"]] %>% 
      dplyr::rename(
        
        Path = "path",
        "\u03B2" = est,
        "\u03B2*" = boot_est,
        "SE*" = boot_se,
        "Lower" = lower_ci,
        "Upper" = upper_ci
        
      )
    
    title_table <- "Loadings Table"
    title_metrics <- "Loadings by Pathway"
    
  }
  
  if(sheet_name == "Weights") {
    
    # Get Weights Table
    model_t <- plssem_mod[["Weights_Table"]] %>% 
      magrittr::set_colnames(
        ., stringr::str_to_title(base::names(.))
      )
    
    # Get Weights Pathway Table
    model_m <- plssem_mod[["Weights"]] %>% 
      dplyr::rename(
        
        Path = "path",
        "\u03B2" = est,
        "\u03B2*" = boot_est,
        "SE*" = boot_se,
        "Lower" = lower_ci,
        "Upper" = upper_ci
        
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
  
  
  # -- WRITE TABLE -- #
  
  openxlsx::writeData(wb,
                      sheet = sheet_name,
                      x = title_table,
                      startCol = start_col,
                      startRow = start_row)
  openxlsx::writeData(wb,
                      sheet = sheet_name,
                      x = model_t,
                      startCol = start_col,
                      startRow = start_row+1)
  
  # NOTE if Weights
  if(sheet_name == "Weights") {
    
    openxlsx::writeData(wb,
                        sheet = sheet_name,
                        x = "NOTE: Weights are normalized to sum to 1.",
                        startCol = start_col,
                        startRow = start_row + 1 + mod_t_row + 1)
    
  }
  
  
  # Apply formatter
  wb <- apply_perf_formatter(
    wb = wb,
    sheet = sheet_name,
    df = model_t,
    start_col = start_col,
    start_row = start_row,
    digits = digits
  )
  
  
  
  # -- WRITE METRICS TABLE -- #
  
  openxlsx::writeData(wb,
                      sheet = sheet_name,
                      x = title_metrics,
                      startCol = start_col,
                      startRow = (start_row + 4 + mod_t_row))
  openxlsx::mergeCells(wb,
                       sheet = sheet_name,
                       cols = 8:9,
                       rows = (start_row + 5 + mod_t_row))
  openxlsx::writeData(wb,
                      sheet = sheet_name,
                      x = "95% CI",
                      startCol = 8,
                      startRow = (start_row + 5 + mod_t_row))
  openxlsx::writeData(wb,
                      sheet = sheet_name,
                      x = model_m,
                      startCol = start_col,
                      startRow = (start_row + 6 + mod_t_row))
  openxlsx::writeData(wb,
                      sheet = sheet_name,
                      x = "NOTE: p-value derived from z-statistic.",
                      startCol = start_col,
                      startRow = (start_row + 6 + mod_t_row + mod_m_row + 1))
  
  # Apply formatter
  wb <- apply_path_formatter(
    
    wb = wb,
    sheet = sheet_name,
    df = model_m,
    start_col = start_col,
    start_row = (start_row + 4 + mod_t_row),
    ci_col = 8:9,
    digits = digits
    
  )
  
  
  
  # Expand column width of col B
  openxlsx::setColWidths(wb,
                         sheet = sheet_name,
                         cols = "B",
                         widths = 30)
  
  # Hide gridlines
  openxlsx::showGridLines(wb,
                          sheet = sheet_name,
                          showGridLines = FALSE)
  
  # Return
  return(wb)
  
}