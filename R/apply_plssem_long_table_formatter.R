#' Apply PLS-SEM Effects Table Formatting
#'
#' @param wb 
#' @param sheet 
#' @param df 
#' @param start_col 
#' @param start_row 
#' @param digits 
#'
#' @export
apply_plssem_long_table_formatter <- function(
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
  header_mod_names_style <- openxlsx::createStyle(
    textDecoration = "bold",
    border = "top",
    halign = "center"
  )
  header_path_names_style <- openxlsx::createStyle(
    textDecoration = "bold",
    halign = "center"
  )
  header_metric_names_style <- openxlsx::createStyle(
    textDecoration = "bold",
    border = "bottom",
    halign = "center"
  )
  header_variable_style <- openxlsx::createStyle(
    textDecoration = "bold",
    border = "bottom"
  )
  
  # Ending style
  end_variable_style <- openxlsx::createStyle(
    textDecoration = "italic",
    border = "bottom"
  )
  end_columns_style <- openxlsx::createStyle(
    border = "bottom",
    halign = "center",
    numFmt = paste0("0.",
                    paste(rep("0",
                              digits),
                          collapse = ""))
  )
  
  # Body style
  body_style <- openxlsx::createStyle(
    halign = "center"
  )
  
  # Rsq style
  rsq_style <- openxlsx::createStyle(
    textDecoration = "italic"
  )
  
  
  
  # -- APPLY STYLES -- #
  
  openxlsx::addStyle(wb,
                     sheet = sheet,
                     style = title_style,
                     cols = start_col,
                     rows = start_row)
  openxlsx::addStyle(wb,
                     sheet = sheet,
                     style = header_mod_names_style,
                     cols = start_col:(mod_col+1),
                     rows = start_row+1)
  openxlsx::addStyle(wb,
                     sheet = sheet,
                     style = header_path_names_style,
                     cols = start_col:(mod_col+1),
                     rows = start_row+2)
  openxlsx::addStyle(wb,
                     sheet = sheet,
                     style = header_metric_names_style,
                     cols = (start_col+1):(mod_col+1),
                     rows = start_row+3)
  openxlsx::addStyle(wb,
                     sheet = sheet,
                     style = header_variable_style,
                     cols = start_col,
                     rows = start_row+3)
  openxlsx::addStyle(wb,
                     sheet = sheet,
                     style = end_variable_style,
                     cols = start_col,
                     rows = start_row + 3 + mod_row)
  openxlsx::addStyle(wb,
                     sheet = sheet,
                     style = end_columns_style,
                     cols = (start_col+1):(mod_col+1),
                     rows = start_row + 3 + mod_row)
  openxlsx::addStyle(wb,
                     sheet = sheet,
                     style = body_style,
                     cols = (start_col+1):(mod_col+1),
                     rows = (start_row + 4):(start_row + mod_row + 2),
                     gridExpand = TRUE)
  openxlsx::addStyle(wb,
                     sheet = sheet,
                     style = rsq_style,
                     cols = start_col,
                     rows = (start_row + mod_row):(start_row + mod_row + 2))
  
}
