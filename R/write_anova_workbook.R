#' Write ANOVA Report Workbook
#'
#' @param anova_object The ANOVA object from \code{wrap_anova}.
#' @param filename The filepath to save the workbook.
#' @param study_name An optional character string indicating the name of the
#'   study.
#' @param assumptions A logical indicating whether to add an ANOVA assumptions
#'   sheet. The default is \code{FALSE}.
#' @param digits A numeric value indicating the number of digits to round to.
#' @param summary_type A numeric value indicating the format of the ANOVA
#'   summary page in the workbook. \code{summary_type = 1} combines the between
#'   and within-subjects ANOVA summaries together. \code{summary_type = 2} keeps
#'   the between and within-subjects ANOVA summaries separate.
#' @param report_es A vector of strings indicating which effect sizes to report.
#'   Options include \code{"ges"} for generalized eta-squared, \code{"etaSq"}
#'   for eta-squared, \code{"etaSqP"} for partial eta-squared, \code{"omegaSq"}
#'   for omega-squared, and \code{"cohen_f"} for Cohen's f. Specify \code{NULL}
#'   to not report any effect size.
#'
#' @export
write_anova <- function(
  anova_object,
  filename,
  study_name = NULL,
  assumptions = FALSE,
  digits = 3,
  summary_type = 1,
  report_es = "etaSqP"
) {
  
  # Create wb
  wb <- openxlsx::createWorkbook()
  
  # Add ANOVA Summary sheet
  add_anova_sum(
    wb,
    sheet_name = "Summary",
    anova_object = anova_object,
    name = study_name,
    digits = digits,
    summary_type = summary_type
  )
  
  # If ANOVA assumptions are specified, then add assumptions sheet
  if (isTRUE(assumptions)) {
    
    add_anova_assumption(
      wb,
      sheet_name = "Assumption",
      anova_object = anova_object,
      name = study_name,
      digits = digits
    )
    
  }
  
  # Add ANOVA Estimated Marginal Means sheet
  add_anova_emm(
    wb,
    sheet_name = "EMM",
    anova_object = anova_object,
    name = study_name,
    digits = digits
  )
  
  # Add ANOVA Estimated Marginal Means Contrasts sheet
  add_anova_emmc(
    wb,
    sheet_name = "Contrasts",
    anova_object = anova_object,
    name = study_name,
    digits = digits
  )
  
  # Add ANOVA Report Table
  add_anova_report(
    wb,
    sheet_name = "Report",
    anova_object = anova_object,
    name = study_name,
    digits = digits,
    report_es = report_es
  )

  
  # Write workbook
  openxlsx::saveWorkbook(
    wb,
    filename,
    overwrite = T
  )
  
}