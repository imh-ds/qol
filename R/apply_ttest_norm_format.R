#' Apply T-Test Norm Assumption Format
#'
#' @param wb An \code{openxlsx} workbook object to apply the format to.
#' @param sheet A character string reflecting the sheet to apply the format to.
#' @param mod_row A numeric value reflecting the n row of the table.
#' @param mod_col A numeric value reflecting the n col of the table.
#' @param start_col A numeric value reflecting the starting column of the table.
#' @param start_row A numeric value reflecting the starting row of the table.
#' @param digits A numeric value indicating the number of digits to round to.
#'
#' @export
apply_ttest_norm_format <- function(
    
  wb,
  sheet,
  mod_row,
  mod_col,
  start_col,
  start_row,
  digits = 3
  
) {
  
  # -- CREATE STYLES -- #
  
  # Title Style
  title_style <- openxlsx::createStyle(
    fontSize = 16,
    textDecoration = "bold"
  )
  
  # Header style
  header_variable_style <- openxlsx::createStyle(
    textDecoration = "bold",
    border = "top-bottom"
  )
  header_p_style <- openxlsx::createStyle(
    textDecoration = c("bold", "italic"),
    border = "top-bottom",
    halign = "center"
  )
  header_columns_style <- openxlsx::createStyle(
    textDecoration = "bold",
    border = "top-bottom",
    halign = "center"
  )
  
  # Ending style
  end_style <- openxlsx::createStyle(
    border = "bottom"
  )
  end_center_style <- openxlsx::createStyle(
    border = "bottom",
    halign = "center"
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
  body_center_style <- openxlsx::createStyle(
    halign = "center"
  )
  body_style <- openxlsx::createStyle(
    halign = "center",
    numFmt = paste0("0.",
                    paste(rep("0",
                              digits),
                          collapse = ""))
  )
  
  
  # -- APPLY STYLES -- #
  
  openxlsx::addStyle(wb,
                     sheet = sheet,
                     style = title_style,
                     cols = start_col,
                     rows = start_row)
  
  
  openxlsx::addStyle(wb,
                     sheet = sheet,
                     style = header_variable_style,
                     cols = start_col,
                     rows = start_row + 1)
  openxlsx::addStyle(wb,
                     sheet = sheet,
                     style = header_columns_style,
                     cols = (start_col + 1):(start_col + mod_col - 1),
                     rows = start_row + 1)
  openxlsx::addStyle(wb,
                     sheet = sheet,
                     style = header_p_style,
                     cols = start_col + mod_col - 1,
                     rows = start_row + 1)
  
  
  openxlsx::addStyle(wb,
                     sheet = sheet,
                     style = body_style,
                     gridExpand = TRUE,
                     cols = (start_col + 2):(start_col + mod_col - 1),
                     rows = (start_row + 2):(start_row + 1 + mod_row))
  openxlsx::addStyle(wb,
                     sheet = sheet,
                     style = body_center_style,
                     gridExpand = TRUE,
                     cols = start_col + 1,
                     rows = (start_row + 2):(start_row + 1 + mod_row))
  
  
  openxlsx::addStyle(wb,
                     sheet = sheet,
                     style = end_style,
                     cols = start_col,
                     rows = start_row + 1 + mod_row)
  openxlsx::addStyle(wb,
                     sheet = sheet,
                     style = end_center_style,
                     cols = start_col + 1,
                     rows = start_row + 1 + mod_row)
  openxlsx::addStyle(wb,
                     sheet = sheet,
                     style = end_columns_style,
                     cols = (start_col + 2):(start_col + mod_col - 1),
                     rows = start_row + 1 + mod_row)
  
}
