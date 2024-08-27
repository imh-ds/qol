#' Add T-Test Summary Sheet
#'
#' @param wb 
#' @param sheet_name 
#' @param ttest_object 
#' @param name 
#' @param digits 
#' 
#' @export
add_ttest_summary <- function(
  
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
  es_name <- dplyr::case_when(
    ttest_object[["meta_data"]][["es_type"]] == "hedges" ~ "g",
    ttest_object[["meta_data"]][["es_type"]] == "cohen" ~ "d",
    ttest_object[["meta_data"]][["es_type"]] == "glass" ~ "\u0394"
  )
  
  es_se_name <- paste0(es_name,
                       " SE")
  
  es_ci_lower <- paste0(es_name,
                        " Lower")
  es_ci_upper <- paste0(es_name,
                        " Upper")
  

  # T-TEST SUMMARY ----------------------------------------------------------

  # Get t-test summary
  sum <- ttest_object[["ttest"]]
  
  sum <- sum %>% 
    
    # Create text reporter
    dplyr::mutate(
      `In Text` = ifelse(
        is.na(test),
        NA,
        paste0(
          "(M diff = ",
          sprintf(rnd, m_dif),
          ifelse(test %in% c("Student's t", "Welch's t"), {
            paste0(", SE diff = ",
                   sprintf(rnd, se_dif),
                   ", t = ",
                   sprintf(rnd, stat))
          }, {
            paste0(", W = ",
                   sprintf(rnd, stat))
          }),
          ", p ",
          ifelse(p < .001, "< .001",
                 paste0("= ", sprintf(rnd, p))),
          ", ",
          ifelse(test %in% c("Student's t", "Welch's t"), {
            paste0(es_name,
                   " = ")
          }, {
            "r = "
          })
          ,
          sprintf(rnd, es),
          ")"
        )
      ),
      outcome = ifelse(!duplicated(outcome), outcome, NA)
    ) %>% 
    
    # Rename
    dplyr::rename(
      Outcome = outcome,
      Test = test,
      Statistic = stat,
      "M Dif" = m_dif,
      "SE Dif" = se_dif,
      Lower = m_ci_lower,
      Upper = m_ci_upper,
      "Effect Size" = es_type
    )
  
  # Separate summary from text reporter
  sum_text <- sum %>% 
    dplyr::select(
      `In Text`
    )
  
  sum <- sum %>% 
    dplyr::select(
      -`In Text`
    )
  
  
  # SUM PARAMETERS ----------------------------------------------------------
  
  # Sum Contrast parameters
  sum_row <- nrow(sum)
  sum_col <- ncol(sum)
  
  
  # WRITE DATA --------------------------------------------------------------
  
  # -- SUMMARY & TITLE -- #
  openxlsx::writeData(
    wb = wb,
    sheet = sheet_name,
    x = if(!is.null(name)) {
      paste0(name,
             " - Independent Samples T-Test")
    } else {
      "Independent Samples T-Test"
    },
    startCol = start_col,
    startRow = start_row
  )
  
  # -- MEAN COMPARISONS -- #
  
  # Merge confidence interval cells
  
  # Mean difference
  openxlsx::mergeCells(
    wb,
    sheet = sheet_name,
    cols = (start_col + sum_col - 10):(start_col + sum_col - 9),
    rows = start_row + 3
  )
  openxlsx::writeData(
    wb,
    sheet = sheet_name,
    x = paste0(
      ttest_object[["meta_data"]][["conf_level"]] * 100,
      "% CI"
    ),
    startCol = start_col + sum_col - 10,
    startRow = start_row + 3
  )
  
  # Effect size
  openxlsx::mergeCells(
    wb,
    sheet = sheet_name,
    cols = (start_col + sum_col - 2):(start_col + sum_col - 1),
    rows = start_row + 3
  )
  openxlsx::writeData(
    wb,
    sheet = sheet_name,
    x = paste0(
      ttest_object[["meta_data"]][["conf_level"]] * 100,
      "% CI"
    ),
    startCol = start_col + sum_col - 2,
    startRow = start_row + 3
  )
  
  
  
  # Title
  openxlsx::writeData(
    wb = wb,
    sheet = sheet_name,
    x = "Summary Table",
    startCol = start_col,
    startRow = start_row + 2
  )
  openxlsx::writeData(
    wb = wb,
    sheet = sheet_name,
    x = sum,
    startCol = start_col,
    startRow = start_row + 4
  )
  
  
  # Fix column names for effect size stat, SE, & CI
  openxlsx::writeData(
    wb = wb,
    sheet = sheet_name,
    x = "Statistic",
    startCol = start_col + sum_col - 4,
    startRow = start_row + 4
  )
  openxlsx::writeData(
    wb = wb,
    sheet = sheet_name,
    x = "SE",
    startCol = start_col + sum_col - 3,
    startRow = start_row + 4
  )
  openxlsx::writeData(
    wb = wb,
    sheet = sheet_name,
    x = "Lower",
    startCol = start_col + sum_col - 2,
    startRow = start_row + 4
  )
  openxlsx::writeData(
    wb = wb,
    sheet = sheet_name,
    x = "Upper",
    startCol = start_col + sum_col - 1,
    startRow = start_row + 4
  )
  
  
  # -- MEAN COMPARISONS TEXTS -- #
  openxlsx::writeData(
    wb = wb,
    sheet = sheet_name,
    x = "Comparisons Text",
    startCol = (start_col + sum_col + 1),
    startRow = start_row + 2
  )
  openxlsx::writeData(
    wb,
    sheet = sheet_name,
    x = sum_text,
    startCol = (start_col + sum_col + 1),
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
  
  # Apply format
  apply_ttest_sum_format(
    wb = wb,
    sheet = sheet_name,
    mod_row = sum_row,
    mod_col = sum_col,
    start_col = start_col,
    start_row = start_row + 2,
    digits = digits
  )
  
  # Apply text format
  apply_text_format(
    wb = wb,
    sheet = sheet_name,
    df = sum_text,
    start_col = (start_col + 1 + sum_col),
    start_row = start_row + 2,
    digits = digits
  )
  
  # Expand column width of col B
  openxlsx::setColWidths(wb,
                         sheet = sheet_name,
                         cols = "B",
                         widths = 15)
  
  # Expand column width of col C
  openxlsx::setColWidths(wb,
                         sheet = sheet_name,
                         cols = "C",
                         widths = 20)
  
  # Expand column width of col K
  openxlsx::setColWidths(wb,
                         sheet = sheet_name,
                         cols = "K",
                         widths = 20)
  
  # Hide gridlines
  openxlsx::showGridLines(wb,
                          sheet = sheet_name,
                          showGridLines = FALSE)
  
}