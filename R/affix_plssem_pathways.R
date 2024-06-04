#' Affix PLS-SEM Model Pathways Workbook Sheet
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
affix_plssem_pathways <- function(
    wb,
    plssem_mod,
    sheet_name,
    digits = 3) {
  
  # Create renamer
  rename_mod <- function(
    model) {
    
    model_num <- model %>% 
      dplyr::select(-c(est,
                       in_text,
                       fig_text)) %>% 
      dplyr::rename(Pathway = path,
                    "\u03B2" = boot_est,
                    SE = boot_se,
                    Lower = lower_ci,
                    Upper = upper_ci)
    
    model_txt <- model %>% 
      dplyr::select(in_text,
                    fig_text) %>% 
      dplyr::rename("In Text" = in_text,
                    "Fig Text" = fig_text)
    
    mod_list <- list(model_num,
                     model_txt)
    
    return(mod_list)
    
  }
  
  # Grab data frames
  model_z <- rename_mod(plssem_mod[["Paths_z"]])[[1]]
  model_z_txt <- rename_mod(plssem_mod[["Paths_z"]])[[2]]
  model_t <- rename_mod(plssem_mod[["Paths_t"]])[[1]]
  model_t_txt <- rename_mod(plssem_mod[["Paths_t"]])[[2]]
  
  # Define rows and columns
  mod_z_row <- nrow(model_z)
  mod_z_col <- ncol(model_z)
  
  mod_zt_row <- nrow(model_z_txt)
  mod_zt_col <- ncol(model_z_txt)
  
  mod_t_row <- nrow(model_t)
  mod_t_col <- ncol(model_t)
  
  mod_tt_row <- nrow(model_t_txt)
  mod_tt_col <- ncol(model_t_txt)
  
  
  # Add model performance worksheet
  openxlsx::addWorksheet(wb,
                         sheet_name)
  
  # Starting column
  start_col = 2
  
  # Starting row
  start_row = 3
  
  
  # -- WRITE Z-STAT PATHWAY TABLE -- #
  
  openxlsx::writeData(wb,
                      sheet = sheet_name,
                      x = "Pathway z-Stat Table",
                      startCol = start_col,
                      startRow = start_row)
  openxlsx::mergeCells(wb,
                       sheet = sheet_name,
                       cols = 6:7,
                       rows = start_row+1)
  openxlsx::writeData(wb,
                      sheet = sheet_name,
                      x = "95% CI",
                      startCol = 6,
                      startRow = start_row+1)
  openxlsx::writeData(wb,
                      sheet = sheet_name,
                      x = model_z,
                      startCol = start_col,
                      startRow = start_row+2)
  
  # Apply formatter
  wb <- apply_path_formatter(
    wb = wb,
    sheet = sheet_name,
    df = model_z,
    start_col = start_col,
    start_row = start_row,
    digits = digits
  )
  
  
  openxlsx::writeData(wb,
                      sheet = sheet_name,
                      x = "Pathway z-Stat Text",
                      startCol = (start_col + 1 + mod_z_col),
                      startRow = start_row)
  openxlsx::writeData(wb,
                      sheet = sheet_name,
                      x = model_z_txt,
                      startCol = (start_col + 1 + mod_z_col),
                      startRow = (start_row + 2))
  
  # Apply formatter
  wb <- apply_text_formatter(
    wb = wb,
    sheet = sheet_name,
    df = model_z_txt,
    start_col = (start_col + 1 + mod_z_col),
    start_row = start_row,
    digits = digits
  )
  
  
  # -- WRITE T-STAT PATHWAY TABLE -- #
  
  openxlsx::writeData(wb,
                      sheet = sheet_name,
                      x = "Pathway t-Stat Table",
                      startCol = start_col,
                      startRow = (start_row + 5 + mod_z_row))
  openxlsx::mergeCells(wb,
                       sheet = sheet_name,
                       cols = 6:7,
                       rows = (start_row + 6 + mod_z_row))
  openxlsx::writeData(wb,
                      sheet = sheet_name,
                      x = "95% CI",
                      startCol = 6,
                      startRow = (start_row + 6 + mod_z_row))
  openxlsx::writeData(wb,
                      sheet = sheet_name,
                      x = model_t,
                      startCol = start_col,
                      startRow = (start_row + 7 + mod_z_row))
  
  # Apply formatter
  wb <- apply_path_formatter(
    wb = wb,
    sheet = sheet_name,
    df = model_t,
    start_col = start_col,
    start_row = (start_row + 5 + mod_z_row),
    digits = digits
  )
  
  openxlsx::writeData(wb,
                      sheet = sheet_name,
                      x = "Pathway t-Stat Text",
                      startCol = (start_col + 1 + mod_z_col),
                      startRow = (start_row + 5 + mod_z_row))
  openxlsx::writeData(wb,
                      sheet = sheet_name,
                      x = model_z_txt,
                      startCol = (start_col + 1 + mod_z_col),
                      startRow = (start_row + 6 + mod_z_row + 1))
  
  # Apply formatter
  wb <- apply_text_formatter(
    wb = wb,
    sheet = sheet_name,
    df = model_t_txt,
    start_col = (start_col + 1 + mod_z_col),
    start_row = (start_row + 5 + mod_z_row),
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
