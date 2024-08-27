#' Apply T-Test Descriptive Format
#'
#' @param wb 
#' @param sheet_name 
#' @param ttest_object 
#' @param name 
#' @param digits 
#'
#' @return
#' @export
#'
#' @examples
add_ttest_descriptives <- function(
    
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
  desc <- ttest_object[["descriptive"]]
  

  # DESCRIPTIVES ------------------------------------------------------------

  # Mutate text reporter
  desc <- desc %>% 
    
    # Mutate texts
    dplyr::mutate(
      
      # Reportable text in format of M ± SD
      "MSD - Style 1" = ifelse(
        is.na(outcome), 
        NA, 
        paste0(
          "(M \u00B1 SD = ",
          sprintf(rnd, mean),
          " \u00B1 ",
          sprintf(rnd, sd),
          ")"
        )
      ),
      
      # Reportable text in format of M, SD
      "MSD - Style 2" = ifelse(
        is.na(outcome),
        NA,
        paste0(
          "(M = ",
          sprintf(rnd, mean),
          ", SD = ",
          sprintf(rnd, sd),
          ")"
        )
      )
      
    ) %>% 
    
    # Remove duplicates
    dplyr::mutate(
      outcome = ifelse(!duplicated(outcome), outcome, NA)
    ) %>% 
    
    # Rename
    dplyr::rename(
      Outcome = outcome,
      Group = group,
      Mean = mean,
      SD = sd,
      SE = se
    )
    
  
  # Separate
  desc_text <- desc %>% 
    dplyr::select(
      "MSD - Style 1",
      "MSD - Style 2"
    )
  
  desc <- desc %>% 
    dplyr::select(
      -c("MSD - Style 1",
         "MSD - Style 2")
    )


  # DESC PARAMETERS ---------------------------------------------------------
  
  # Descriptive parameters
  desc_row <- nrow(desc)
  desc_col <- ncol(desc)
  
  # Descriptive text parameters
  desc_text_row <- nrow(desc_text)
  desc_text_col <- ncol(desc_text)
  
  
  # WRITE DATA --------------------------------------------------------------
  
  # -- SUMMARY & TITLE -- #
  openxlsx::writeData(
    wb = wb,
    sheet = sheet_name,
    x = if(!is.null(name)) {
      paste0(name,
             " - Descriptive Statistics")
    } else {
      "Descriptive Statistics"
    },
    startCol = start_col,
    startRow = start_row
  )
  
  
  # Title
  openxlsx::writeData(
    wb = wb,
    sheet = sheet_name,
    x = "Descriptive Table",
    startCol = start_col,
    startRow = start_row + 2
  )
  openxlsx::writeData(
    wb = wb,
    sheet = sheet_name,
    x = desc,
    startCol = start_col,
    startRow = start_row + 3
  )
  
  
  
  # -- DESCRIPTIVE TEXTS -- #
  openxlsx::writeData(
    wb = wb,
    sheet = sheet_name,
    x = "Descriptive Text",
    startCol = (start_col + desc_col + 1),
    startRow = start_row + 2
  )
  openxlsx::writeData(
    wb,
    sheet = sheet_name,
    x = desc_text,
    startCol = (start_col + desc_col + 1),
    startRow = start_row + 3
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
  
  # Apply format
  apply_ttest_desc_format(
    wb = wb,
    sheet = sheet_name,
    mod_row = desc_row,
    mod_col = desc_col,
    start_col = start_col,
    start_row = start_row + 2,
    digits = digits
  )
  
  # Apply text format
  apply_ttest_desc_text_format(
    wb = wb,
    sheet = sheet_name,
    mod_row = desc_text_row,
    mod_col = desc_text_col,
    start_col = (start_col + 1 + desc_col),
    start_row = start_row + 2,
    digits = digits
  )
  
  # Expand column width of col B
  openxlsx::setColWidths(wb,
                         sheet = sheet_name,
                         cols = "B",
                         widths = 15)
  
  # Hide gridlines
  openxlsx::showGridLines(wb,
                          sheet = sheet_name,
                          showGridLines = FALSE)
  
}

