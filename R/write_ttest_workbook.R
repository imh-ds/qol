#' Write T-Test Report Workbook
#'
#' @param ttest_object The t-test object from \code{wrap_ttest}.
#' @param filename The filepath to save the workbook.
#' @param study_name An optional character string indicating the name of the
#'   study.
#' @param assumptions A logical indicating whether to add an t-test assumptions
#'   sheet. The default is \code{FALSE}.
#' @param digits A numeric value indicating the number of digits to round to.
#' @param report_variance A string indicating w
#'
#' @export
write_ttest <- function(
  ttest_object,
  filename,
  study_name = NULL,
  assumptions = FALSE,
  digits = 3,
  report_variance = "sd",
  report_test = "welch"
) {
  
  # Create wb
  wb <- openxlsx::createWorkbook()
  
  # Add t-test Summary Sheet
  add_ttest_summary(
    wb = wb,
    sheet_name = "Summary",
    ttest_object = ttest_object,
    name = study_name,
    digits = digits
  )
  
  # If t-test assumptions are specified, then add assumptions sheet
  if (isTRUE(assumptions)) {
    add_ttest_assumptions(
      wb = wb,
      sheet_name = "Assumption",
      ttest_object = ttest_object,
      name = study_name,
      digits = digits
    )
  }
  
  # Add t-test Descriptive sheet
  add_ttest_descriptives(
    wb = wb,
    sheet_name = "Descriptives",
    ttest_object = ttest_object,
    name = study_name,
    digits = digits
  )
  
  # Add t-test Report Table
  add_ttest_report(
    wb = wb,
    sheet_name = "Report",
    ttest_object = ttest_object,
    name = study_name,
    digits = digits,
    report_variance = report_variance,
    report_test = report_test
  )
  
  
  # Write workbook
  openxlsx::saveWorkbook(
    wb,
    file = filename,
    overwrite = T
  )
  
}