#' Write PLS-SEM Report Workbook
#'
#' @param plssem_object The PLS-SEM object from \code{wrap_plssem}.
#' @param filename The filepath to save the workbook.
#' @param study_name An optional character string indicating the name of the
#'   study.
#' @param digits A numeric value indicating the number of digits to round to.
#' @param path_tables A vector of strings indicating which direct effects
#'   pathway table to export. Options include \code{"long"} for long-formatted
#'   regression tables with confidence intervals below the coefficients,
#'   \code{"wide"} for wide-formatted regression tables with confidence
#'   intervals adjacent to the coefficients, or \code{NULL} to not export any.
#'
#' @export
write_plssem_workbook <- function(
    plssem_object,
    filename,
    study_name = NULL,
    digits = 3,
    path_tables = c("long", "wide")
) {
  
  # Create a workbook
  wb <- openxlsx::createWorkbook()
  
  # Add PLS-SEM Model Diagnostics
  add_plssem_diagnostic(
    wb,
    plssem_mod = plssem_object,
    sheet_name = "Diagnostic",
    name = study_name,
    digits = digits
  )
  
  # Add PLS-SEM Pathways
  add_plssem_pathways(
    wb,
    plssem_mod = plssem_object,
    sheet_name = "Paths",
    name = study_name,
    digits = digits
  )
  
  # Add PLS-SEM Loadings
  add_plssem_load_wgt(
    wb,
    plssem_mod = plssem_object,
    sheet_name = "Loadings",
    name = study_name,
    digits = digits
  )
  
  # Add PLS-SEM Weights
  add_plssem_load_wgt(
    wb,
    plssem_mod = plssem_object,
    sheet_name = "Weights",
    name = study_name,
    digits = digits
  )
  
  # Add PLS-SEM Regression tables if specified
  
  # If long table
  if ("long" %in% path_tables) {
    
    add_plssem_long_table(
      wb,
      plssem_mod = plssem_object,
      sheet_name = "Table (Long)",
      name = study_name,
      digits = digits
    )
    
  }
  
  # If wide table
  if ("wide" %in% path_tables) {
    
    add_plssem_wide_table(
      wb,
      plssem_mod = plssem_object,
      sheet_name = "Table (Wide)",
      name = study_name,
      digits = digits
    )
    
  }
  
  # If prioritization analysis was done, add prioritization sheet
  if (!is.null(plssem_object[["Prioritization"]])) {
    
    add_plssem_prioritization(
      wb,
      plssem_mod = plssem_object,
      sheet_name = "Priority",
      name = study_name,
      digits = digits
    )
    
  }
  
  
  # Write file
  openxlsx::saveWorkbook(
    wb = wb,
    file = filename,
    overwrite = TRUE
  )
  
}