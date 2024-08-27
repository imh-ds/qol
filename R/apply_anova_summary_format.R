#' Apply ANOVA Summary Formatting
#'
#' @param wb 
#' @param sheet 
#' @param mod_row 
#' @param mod_col 
#' @param start_col 
#' @param start_row 
#' @param digits 
#'
#' @export
apply_anova_summary_formatter <- function(
    
  wb,
  sheet,
  mod_row,
  mod_col,
  start_col,
  start_row,
  digits = 3,
  correction = FALSE,
  bs_row = NULL,
  ws_row = NULL
  
) {
  
  # Correct the correction logic if ws_row is given
  if (!is.null(ws_row)) {
    
    correction <- TRUE
    
  }
  
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
  header_columns_style <- openxlsx::createStyle(
    textDecoration = "bold",
    border = "top-bottom",
    halign = "center"
  )
  header_metric_style <- openxlsx::createStyle(
    textDecoration = c("bold", "italic"),
    border = "top-bottom",
    halign = "center"
  )
  
  # Ending style
  end_df_style <- openxlsx::createStyle(
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
  if (!is.null(ws_row)) {
    
    end_variable_style <- openxlsx::createStyle(
      border = "bottom",
      indent = 1
    )
    
  } else {
    
    end_variable_style <- openxlsx::createStyle(
      border = "bottom"
    )
    
  }
  
  # Variable style
  if (!is.null(ws_row)) {
    
    variable_indent_style <- openxlsx::createStyle(
      indent = 1
    )
    variable_effect_style <- openxlsx::createStyle(
      textDecoration = "bold"
    )
    
  }
  
  # Body style
  body_df_style <- openxlsx::createStyle(
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
                     cols = (start_col + 1):(start_col + 3),
                     rows = start_row + 1)
  openxlsx::addStyle(wb,
                     sheet = sheet,
                     style = header_metric_style,
                     cols = (start_col + 4):(start_col + 5),
                     rows = start_row + 1)
  openxlsx::addStyle(wb,
                     sheet = sheet,
                     style = header_columns_style,
                     cols = (start_col + 6):(start_col + mod_col - 1),
                     rows = start_row + 1)
  openxlsx::addStyle(wb,
                     sheet = sheet,
                     style = body_style,
                     gridExpand = TRUE,
                     cols = (start_col + 1):(start_col + mod_col),
                     rows = (start_row + 2):(start_row + 1 + mod_row))
  if (isFALSE(correction)) {
    openxlsx::addStyle(wb,
                       sheet = sheet,
                       style = body_df_style,
                       cols = start_col + 2,
                       rows = (start_row + 2):(start_row + 1 + mod_row))
  }
  openxlsx::addStyle(wb,
                     sheet = sheet,
                     style = end_columns_style,
                     cols = (start_col + 1):(start_col + mod_col - 1),
                     rows = start_row + mod_row + 1)
  if (isFALSE(correction)) {
    openxlsx::addStyle(wb,
                       sheet = sheet,
                       style = end_df_style,
                       cols = start_col + 2,
                       rows = start_row + mod_row + 1)
  }
  
  
  # If there are indirect effects, apply additional formatting
  if (!is.null(ws_row)) {
    
    # Within Subjects Effects
    openxlsx::addStyle(wb,
                       sheet = sheet,
                       style = variable_indent_style,
                       cols = start_col,
                       rows = (start_row + 3):(start_row + 3 + ws_row))
    
    # Between Subjects Effects
    openxlsx::addStyle(wb,
                       sheet = sheet,
                       style = variable_indent_style,
                       cols = start_col,
                       rows = (start_row + 5 + ws_row):(start_row + 3 + ws_row + bs_row))
    
    # Within Subjects Effect Label
    openxlsx::addStyle(wb,
                       sheet = sheet,
                       style = variable_effect_style,
                       cols = start_col,
                       rows = start_row + 2)
    
    # Between Subjects Effect Label
    openxlsx::addStyle(wb,
                       sheet = sheet,
                       style = variable_effect_style,
                       cols = start_col,
                       rows = start_row + 4 + ws_row)
    
  }
  
  openxlsx::addStyle(wb,
                     sheet = sheet,
                     style = end_variable_style,
                     cols = start_col,
                     rows = start_row + mod_row + 1)
  
}