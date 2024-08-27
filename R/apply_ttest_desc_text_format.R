#' Apply T-Test Descriptive Text Formatting
#'
#' @param wb 
#' @param sheet 
#' @param df 
#' @param start_col 
#' @param start_row 
#'
#' @export
apply_ttest_desc_text_format <- function(
    wb,
    sheet,
    start_col,
    start_row,
    mod_row,
    mod_col,
    digits = 3) {
  
  # -- CREATE STYLES -- #
  
  # Title Style
  title_style <- openxlsx::createStyle(
    fontSize = 16,
    textDecoration = "bold"
  )
  
  # Header style
  header_style <- openxlsx::createStyle(
    textDecoration = "bold",
    border = "top-bottom"
  )
  
  # Ending style
  end_variable_style <- openxlsx::createStyle(
    border = "bottom"
  )
  
  # -- APPLY STYLES -- #
  
  openxlsx::addStyle(wb,
                     sheet = sheet,
                     style = title_style,
                     cols = start_col,
                     rows = start_row)
  openxlsx::addStyle(wb,
                     sheet = sheet,
                     style = header_style,
                     cols = (start_col):(start_col + mod_col - 1),
                     rows = (start_row + 1))
  openxlsx::addStyle(wb,
                     sheet = sheet,
                     style = end_variable_style,
                     cols = (start_col):(start_col + mod_col - 1),
                     rows = (start_row + 1 + mod_row))
  
  # Expand column width of col
  openxlsx::setColWidths(wb,
                         sheet = sheet,
                         cols = start_col:(start_col+1),
                         widths = 25)
  
}
