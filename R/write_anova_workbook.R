#' Write ANOVA Report Workbook
#'
#' @param anova_object The ANOVA object from \code{wrap_anova}.
#' @param filename The filepath to save the workbook.
#' @param study_name An optional character string indicating the name of the
#'   study.
#' @param assumptions A logical indicating whether to add an ANOVA assumptions
#'   sheet. The default is FALSE.
#' @param digits A numeric value indicating the number of digits to round to.
#'
#' @export
write_anova <- function(
    anova_object,
    filename,
    study_name = NULL,
    assumptions = FALSE,
    digits = 3,
    summary_type = 1,
    report_es = NULL
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