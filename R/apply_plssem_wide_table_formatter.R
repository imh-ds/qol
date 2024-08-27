#' Title
#'
#' @param wb 
#' @param sheet 
#' @param df 
#' @param paths 
#' @param start_col 
#' @param start_row 
#' @param digits 
#' 
#' @export
apply_wide_table_formatter <- function(
    wb,
    sheet,
    df,
    paths,
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
    border = "bottom",
    halign = "center"
  )
  header_ci_style <- openxlsx::createStyle(
    textDecoration = "bold",
    border = "bottom",
    halign = "center"
  )
  header_metric_names_style <- openxlsx::createStyle(
    textDecoration = "bold",
    border = "bottom",
    halign = "center"
  )
  header_p_style <- openxlsx::createStyle(
    textDecoration = c("bold", "italic"),
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
    halign = "center",
    numFmt = paste0("0.",
                    paste(rep("0",
                              digits),
                          collapse = ""))
  )
  body_style_df <- openxlsx::createStyle(
    halign = "center",
    numFmt = "0"
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
                     cols = (start_col+1):(mod_col+1),
                     rows = start_row+2)
  
  for(i in 0:(length(paths)-1)) {
    startCol <- 4 + i*4
    endCol <- startCol + 1
    openxlsx::addStyle(wb,
                       sheet,
                       style = header_ci_style,
                       rows = start_row+3,
                       cols = startCol:endCol)
  }
  
  openxlsx::addStyle(wb,
                     sheet = sheet,
                     style = header_metric_names_style,
                     cols = (start_col+1):(mod_col+1),
                     rows = start_row+4)
  
  for(i in 0:(length(paths)-1)) {
    startCol <- 6 + i*4
    openxlsx::addStyle(wb,
                       sheet,
                       style = header_p_style,
                       rows = start_row+4,
                       cols = startCol)
  }
  
  openxlsx::addStyle(wb,
                     sheet = sheet,
                     style = header_variable_style,
                     cols = start_col,
                     rows = start_row+4)
  openxlsx::addStyle(wb,
                     sheet = sheet,
                     style = end_variable_style,
                     cols = start_col,
                     rows = start_row + 4 + mod_row)
  openxlsx::addStyle(wb,
                     sheet = sheet,
                     style = end_columns_style,
                     cols = (start_col+1):(mod_col+1),
                     rows = start_row + 4 + mod_row)
  openxlsx::addStyle(wb,
                     sheet = sheet,
                     style = body_style,
                     cols = (start_col+1):(mod_col+1),
                     rows = (start_row + 5):(start_row + mod_row + 3),
                     gridExpand = TRUE)
  openxlsx::addStyle(wb,
                     sheet = sheet,
                     style = body_style_df,
                     cols = (start_col+1):(mod_col+1),
                     rows = start_row + 1 + mod_row,
                     gridExpand = TRUE)
  openxlsx::addStyle(wb,
                     sheet = sheet,
                     style = rsq_style,
                     cols = start_col,
                     rows = (start_row + mod_row + 1):(start_row + mod_row + 3))
  
  # Return
  return(wb)
  
}