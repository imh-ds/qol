#' Apply Text Table Formatting
#'
#' @param wb 
#' @param sheet 
#' @param df 
#' @param start_col 
#' @param start_row 
#'
#' @export
apply_text_format <- function(
    wb,
    sheet,
    df,
    start_col,
    start_row,
    digits = 3) {
  
  # Define rows and columns
  mod_row <- nrow(df)
  mod_col <- ncol(df)
  
  # -- CREATE STYLES -- #
  
  # Title Style
  title_style <- openxlsx::createStyle(
    fontSize = 16,
    textDecoration = "bold"
  )
  
  # Header style
  header_top_style <- openxlsx::createStyle(
    textDecoration = "bold",
    border = "top"
  )
  header_bottom_style <- openxlsx::createStyle(
    textDecoration = "bold",
    border = "bottom"
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
                     style = header_top_style,
                     cols = (start_col):(start_col + mod_col - 1),
                     rows = (start_row + 1))
  openxlsx::addStyle(wb,
                     sheet = sheet,
                     style = header_bottom_style,
                     cols = (start_col):(start_col + mod_col - 1),
                     rows = (start_row + 2))
  openxlsx::addStyle(wb,
                     sheet = sheet,
                     style = end_variable_style,
                     cols = (start_col):(start_col + mod_col - 1),
                     rows = (start_row + 2 + mod_row))
  
  # Expand column width of col
  openxlsx::setColWidths(wb,
                         sheet = sheet,
                         cols = start_col:(start_col+1),
                         widths = "auto")
  
}
