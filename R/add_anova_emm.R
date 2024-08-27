#' Add ANOVA Estimated Marginal Means Sheet
#'
#' @param wb An \code{openxlsx} workbook object to add the sheet to.
#' @param sheet_name A character string reflecting the name of the worksheet.
#' @param anova_object The ANOVA object from \code{anova_wrapper}.
#' @param digits A numeric value indicating the number of digits to round to.
#' @param name An optional character string reflecting the name of the study.
#'
#' @export
add_anova_emm <- function(
    wb,
    sheet_name,
    anova_object,
    digits = 3,
    name = NULL
) {
  
  # PARAMETERS --------------------------------------------------------------
  
  # Starting column
  start_col = 2
  
  # Starting row
  start_row = 3
  
  
  # ESTIMATED MARGINAL MEANS ------------------------------------------------
  
  emm <- anova_object[["emm"]] %>% 
    
    # Mutate
    dplyr::mutate(
      dplyr::across(.cols = dplyr::everything(),
                    .fns = function(x) ifelse(!is.na(effect) & is.na(x), 
                                              "-", 
                                              x)),
      effect = ifelse(duplicated(effect), NA, effect),
      effect = gsub("\\*", " \u00D7 ", effect)
    ) %>% 
    
    # Rename
    dplyr::rename(
      Effect = effect,
      Mean = mean,
      SE = se,
      Lower = lower,
      Upper = upper
    )
  
  # Create worksheet
  openxlsx::addWorksheet(
    wb,
    sheetName = sheet_name
  )
  
  
  # EMM PARAMETERS ----------------------------------------------------------
  
  # EMM parameters
  emm_row <- nrow(emm)
  emm_col <- ncol(emm)
  
  
  # WRITE DATA --------------------------------------------------------------
  
  # -- SUMMARY & TITLE -- #
  openxlsx::writeData(
    wb = wb,
    sheet = sheet_name,
    x = if(!is.null(name)) {
      paste0(name,
             " - Estimated Marginal Means")
    } else {
      "Estimated Marginal Means"
    },
    startCol = start_col,
    startRow = start_row
  )
  
  # -- EMM DESCRIPTIVES -- #
  openxlsx::mergeCells(
    wb,
    sheet = sheet_name,
    cols = (start_col + emm_col - 2):(start_col + emm_col - 1),
    rows = start_row + 3
  )
  openxlsx::writeData(
    wb,
    sheet = sheet_name,
    x = paste0(
      anova_object[["meta_data"]][["conf_level"]] * 100,
      "% CI"
    ),
    startCol = start_col + emm_col - 2,
    startRow = start_row + 3
  )
  openxlsx::writeData(
    wb = wb,
    sheet = sheet_name,
    x = "Descriptives",
    startCol = start_col,
    startRow = start_row + 2
  )
  openxlsx::writeData(
    wb = wb,
    sheet = sheet_name,
    x = emm,
    startCol = start_col,
    startRow = start_row + 4
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
  
  # Apply summary to first summary
  apply_anova_emm_format(
    wb = wb,
    sheet = sheet_name,
    mod_row = emm_row,
    mod_col = emm_col,
    start_col = start_col,
    start_row = start_row + 2,
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
  
}