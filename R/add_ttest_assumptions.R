#' Apply T-Test Assumption Format
#'
#' @param wb 
#' @param sheet_name 
#' @param ttest_object 
#' @param name 
#' @param digits 
#'
#' @export
add_ttest_assumptions <- function(
    
  wb,
  sheet_name,
  ttest_object,
  name = NULL,
  digits = 3
  
) {
  
  # PARAMETERS --------------------------------------------------------------
  
  # Create worksheet
  openxlsx::addWorksheet(
    wb,
    sheetName = sheet_name
  )
  
  # Rounding parameter
  rnd <- paste0(
    '%.',
    digits,
    'f'
  )
  
  # Starting column
  start_col = 2
  
  # Starting row
  start_row = 3
  
  # Get names of effect size
  norm <- ttest_object[["normality"]]
  hvar <- ttest_object[["equality"]]
  
  

  # ASSUMPTIONS -------------------------------------------------------------

  # -- TEST OF NORMALITY -- #
  
  norm <- norm %>% 
    
    # Remove duplicates
    dplyr::mutate(
      outcome = ifelse(!duplicated(outcome), outcome, NA)
    ) %>% 
    
    # Rename
    dplyr::rename(
      Outcome = outcome,
      Group = group
    )
  
  
  # -- TEST OF HOMOGENEITY OF VARIANCE -- #
  hvar <- hvar %>% 
    
    # Remove duplicates
    dplyr::mutate(
      outcome = ifelse(!duplicated(outcome), outcome, NA)
    ) %>% 
    
    # Rename
    dplyr::rename(
      Outcome = outcome,
      Test = test,
      Center = center,
      df1 = df_num,
      df2 = df_den
    )
  
  
  
  # ASSM PARAMETERS ---------------------------------------------------------
  
  # Normality parameters
  norm_row <- nrow(norm)
  norm_col <- ncol(norm)
  
  # Homogeneity of Variance parameters
  hvar_row <- nrow(hvar)
  hvar_col <- ncol(hvar)
  
  
  
  # WRITE DATA --------------------------------------------------------------
  
  # -- TITLE -- #
  openxlsx::writeData(
    wb = wb,
    sheet = sheet_name,
    x = if(!is.null(name)) {
      paste0(name,
             " - Assumptions")
    } else {
      "Assumptions"
    },
    startCol = start_col,
    startRow = start_row
  )
  
  
  # Homogeneity of Variance
  openxlsx::writeData(
    wb = wb,
    sheet = sheet_name,
    x = "Homogeneity of Variance | Levene's & Brown-Forsythe Test",
    startCol = start_col,
    startRow = start_row + 2
  )
  openxlsx::writeData(
    wb = wb,
    sheet = sheet_name,
    x = hvar,
    startCol = start_col,
    startRow = start_row + 3
  )
  
  
  # Normality Test
  openxlsx::writeData(
    wb = wb,
    sheet = sheet_name,
    x = "Normality | Shapiro-Wilk",
    startCol = start_col,
    startRow = start_row + 2 + hvar_row + 4
  )
  openxlsx::writeData(
    wb = wb,
    sheet = sheet_name,
    x = norm,
    startCol = start_col,
    startRow = start_row + 3 + hvar_row + 4
  )
  
  
  # APPLY FORMATTING --------------------------------------------------------
  
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
  
  # Apply format to variance assumption
  apply_ttest_var_format(
    wb = wb,
    sheet = sheet_name,
    mod_row = hvar_row,
    mod_col = hvar_col,
    start_col = start_col,
    start_row = start_row + 2,
    digits = digits
  )
  
  # Apply format to normality / sphericity assumption
  apply_ttest_norm_format(
    wb = wb,
    sheet = sheet_name,
    mod_row = norm_row,
    mod_col = norm_col,
    start_col = start_col,
    start_row = start_row + 2 + hvar_row + 4
  )
  
  # Expand column width of col B
  openxlsx::setColWidths(wb,
                         sheet = sheet_name,
                         cols = "B",
                         widths = 20)
  
  # Expand column width of col C
  openxlsx::setColWidths(wb,
                         sheet = sheet_name,
                         cols = "C",
                         widths = 15)
  
  # Hide gridlines
  openxlsx::showGridLines(wb,
                          sheet = sheet_name,
                          showGridLines = FALSE)
  
}

