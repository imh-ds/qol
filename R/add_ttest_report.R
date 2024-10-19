#' Add T-Test Reportable Table Sheet
#'
#' @param wb An \code{openxlsx} workbook object to apply the format to.
#' @param sheet_name A character string reflecting the sheet.
#' @param ttest_object The t-test object from \code{wrap_ttest}.
#' @param name An optional character string indicating the name of the study.
#' @param digits A numeric value indicating the number of digits to round to.
#' @param report_variance A string value or vector of strings indicating which
#'   variance measure to report. Options include \code{"sd"} for standard
#'   deviation, \code{"se"} for standard error, or \code{c("sd", "se")} for
#'   both. The default is standard deviation \code{"sd"}.
#' @param report_test A string value indicating which t-test statistic to
#'   report. Options include \code{"student"} for Student's t-test,
#'   \code{"welch"} for Welch's t-test, and \code{"wilcox"} for Wilcoxon
#'   Rank-Sum W test / Mann-Whitney U test. The default is \code{"welch"}.
#'
add_ttest_report <- function(
  wb,
  sheet_name,
  ttest_object,
  name = NULL,
  digits = 3,
  report_variance = "sd",
  report_test = "welch"
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
  
  # Generate t-test report
  report_df <- generate_ttest_report(
    ttest_object = ttest_object,
    report_variance = report_variance,
    report_test = report_test
  )

  
  # TABLE PARAMETERS --------------------------------------------------------
  
  rep_row <- nrow(report_df)
  rep_col <- ncol(report_df)
  
  
  # WRITE DATA --------------------------------------------------------------
  
  # -- SUMMARY & TITLE -- #
  openxlsx::writeData(
    wb = wb,
    sheet = sheet_name,
    x = if(!is.null(name)) {
      paste0(name,
             " - Independent Samples T-Test Report")
    } else {
      "Independent Samples T-Test Report"
    },
    startCol = start_col,
    startRow = start_row
  )
  
  
  # Title
  openxlsx::writeData(
    wb = wb,
    sheet = sheet_name,
    x = "Reportable Table",
    startCol = start_col,
    startRow = start_row + 2
  )
  openxlsx::writeData(
    wb = wb,
    sheet = sheet_name,
    x = report_df,
    startCol = start_col,
    startRow = start_row + 3
  )
  
  
  # Write Note
  openxlsx::writeData(
    wb = wb,
    sheet = sheet_name,
    x = paste0(
      "NOTE: Running ",
      ttest_type,
      " test; Effect size is ",
      es_stat,
      "."
    ),
    startCol = start_col,
    startRow = start_row + 3 + rep_row + 1
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
  apply_ttest_report_format(
    wb = wb,
    sheet = sheet_name,
    mod_row = rep_row,
    mod_col = rep_col,
    start_col = start_col,
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


#' Generate T-Test Reportable Table Sheet
#'
#' @param ttest_object The t-test object from \code{wrap_ttest}.
#' @param report_variance A string value or vector of strings indicating which
#'   variance measure to report. Options include \code{"sd"} for standard
#'   deviation, \code{"se"} for standard error, or \code{c("sd", "se")} for
#'   both. The default is standard deviation \code{"sd"}.
#' @param report_test A string value indicating which t-test statistic to
#'   report. Options include \code{"student"} for Student's t-test,
#'   \code{"welch"} for Welch's t-test, and \code{"wilcox"} for Wilcoxon
#'   Rank-Sum W test / Mann-Whitney U test. The default is \code{"welch"}.
#'
generate_ttest_report <- function(
    ttest_object,
    report_variance = "sd",
    report_test = "welch"
) {
  
  # Get names of effect size
  desc <- ttest_object[["descriptive"]]
  
  
  # t-test type
  ttest_type <- dplyr::case_when(
    report_test == "student" ~ "Student's t",
    report_test == "welch" ~ "Welch's t",
    report_test == "wilcox" ~ "Wilcoxon Rank-Sum W"
  )
  
  ttest_stat <- dplyr::case_when(
    report_test %in% c("student", "welch") ~ "t",
    report_test == "wilcox" ~ "W"
  )
  
  
  # Get names of effect size
  es_name <- dplyr::case_when(
    ttest_object[["meta_data"]][["es_type"]] == "hedges" ~ "g",
    ttest_object[["meta_data"]][["es_type"]] == "cohen" ~ "d",
    ttest_object[["meta_data"]][["es_type"]] == "glass" ~ "\u0394"
  )
  
  es_stat <- dplyr::case_when(
    ttest_object[["meta_data"]][["es_type"]] == "hedges" ~ "Hedge's g",
    ttest_object[["meta_data"]][["es_type"]] == "cohen" ~ "Cohen's d",
    ttest_object[["meta_data"]][["es_type"]] == "glass" ~ "Glass's \u0394"
  )
  
  
  # Create t-test statistics
  ttest_stats <- ttest_object[["ttest"]] %>% 
    
    # Filter to selected t-test type
    dplyr::filter(test == !!ttest_type) %>% 
    
    # Select relevant variables
    dplyr::select(
      outcome, 
      df, 
      stat, 
      p, 
      es
    )
  
  
  # If standard error is NOT selected:
  if (!"se" %in% report_variance) {
    
    ttest_desc <- ttest_object[["descriptive"]] %>% 
      dplyr::select(-se) %>% 
      dplyr::rename(SD = sd)
    
  }
  
  # If standard deviation is NOT selected:
  if (!"sd" %in% report_variance) {
    
    ttest_desc <- ttest_object[["descriptive"]] %>% 
      dplyr::select(-sd) %>% 
      dplyr::rename(SE = se)
    
  }
  
  # If both standard error and deviation are selected:
  if (all(c("sd", "se") %in% report_variance)) {
    
    ttest_desc <- ttest_object[["descriptive"]] %>% 
      dplyr::rename(SD = sd,
                    SE = se)
    
  }
  
  
  # Create report data frame
  report_df <- ttest_desc %>% 
    
    # Remove duplicated outcomes
    dplyr::mutate(
      outcome = ifelse(!duplicated(outcome), 
                       outcome, 
                       NA)
    ) %>% 
    
    # Join with t-test statistics
    dplyr::full_join(
      ttest_stats, 
      by = "outcome"
    ) %>% 
    
    # Rename
    dplyr::rename(
      Outcome = outcome,
      Group = group,
      Mean = mean,
      !!rlang::sym(ttest_stat) := stat,
      !!rlang::sym(es_name) := es
    )
  
  # Return
  return(report_df)
  
}