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
  digits = 3  
  
) {
  
  # Get table
  table <- plssem_mod[["Tables_Long"]]
  
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
  title_table <- "PLS-SEM Direct Effects Table - Long Format"
  
  # Add model worksheet
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
                      x = header_names,
                      startCol = start_col,
                      startRow = start_row+1)
  openxlsx::writeData(wb,
                      sheet = sheet_name,
                      x = "",
                      startCol = start_col,
                      startRow = start_row+1)
  openxlsx::writeData(wb,
                      sheet = sheet_name,
                      x = table,
                      startCol = start_col,
                      startRow = start_row+3)
  openxlsx::writeData(wb,
                      sheet = sheet_name,
                      x = "NOTE: All effects are bootstrapped. *** p < .001, ** p < .01, * p < .05.",
                      startCol = start_col,
                      startRow = start_row + 3 + mod_row + 1)
  
  # Apply formatter
  apply_long_table_formatter(
    wb = wb,
    sheet = sheet_name,
    df = table,
    start_col = start_col,
    start_row = start_row,
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