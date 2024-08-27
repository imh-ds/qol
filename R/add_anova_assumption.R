#' Add ANOVA Estimated Marginal Means Interaction Sheet
#'
#' @param wb An \code{openxlsx} workbook object to add the sheet to.
#' @param sheet_name A character string reflecting the name of the worksheet.
#' @param anova_object The ANOVA object from \code{anova_wrapper}.
#' @param digits A numeric value indicating the number of digits to round to.
#' @param name An optional character string reflecting the name of the study.
#'
#' @export
add_anova_assumption <- function(
    wb,
    sheet_name,
    anova_object,
    digits = 3,
    name = NULL,
    within = FALSE
) {
  
  # PARAMETERS --------------------------------------------------------------
  
  # Create worksheet
  openxlsx::addWorksheet(
    wb,
    sheetName = sheet_name
  )
  
  # Rename functions
  rename_norm <- function(norm) {
    
    norm <- norm %>% 
      dplyr::rename(
        Variable = outcome,
        W = s
      )
    
    # Return
    return(norm)
    
  }
  rename_var <- function(var) {
    
    var <- var %>% 
      dplyr::rename(
        Variable = outcome
      )
    
  }
  rename_sph <- function(sph) {
    
    sph <- sph %>% 
      dplyr::rename(
        Variable = outcome,
        "W" = mauch,
        "Greenhouse-Geisser \u03B5" = gg,
        "Huynh-Feldt \u03B5" = hf
      )
    
  }
  
  # Type
  type <- anova_object[["meta_data"]][["anova_type"]]
  
  # Type logic
  between <- type %in% c("One-way", "Factorial")
  within <- type %in% c("Repeated Measures", "Mixed Model")
  
  # Starting column
  start_col = 2
  
  # Starting row
  start_row = 3
  
  
  # GET ASSUMPTIONS ---------------------------------------------------------
  
  # Levene's Test for Homogeneity of Variance Test
  var <- if (isTRUE(between)) {
    
    rename_var(anova_object[["bs_var"]])
    
  } else {
    
    rename_var(anova_object[["rm_var"]])
    
  }
  
  
  # If between-subjects ANOVA, get Shapiro-Wilk Test for Normality
  norm <- if (isTRUE(between)) {
    
    rename_norm(anova_object[["bs_norm"]])
    
  } else {NULL}
  
  
  # If within-subjects ANOVA, get Mauchly Test of Sphericity
  sph <- if (isTRUE(within)) {
    
    rename_sph(anova_object[["rm_sph"]])
    
  } else {NULL}
  
  
  
  # ASSUMPTION PARAMETERS ---------------------------------------------------
  
  # Get rows and columns of Homogeneity of Variance
  var_row <- nrow(var)
  var_col <- ncol(var)
  
  # Get rows and columns of Normality Test / Sphericity Test
  nsp_row <- if (isTRUE(between)) {
    nrow(norm)
  } else {
    nrow(sph)
  }
  nsp_col <- if (isTRUE(between)) {
    ncol(norm)
  } else {
    ncol(sph)
  }
  
  
  
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
    x = "Homogeneity of Variance | Levene's Test",
    startCol = start_col,
    startRow = start_row + 2
  )
  openxlsx::writeData(
    wb = wb,
    sheet = sheet_name,
    x = var,
    startCol = start_col,
    startRow = start_row + 3
  )
  
  
  # -- BETWEEN-SUBJECTS -- #
  if (isTRUE(between)) {
    
    # Normality Test
    openxlsx::writeData(
      wb = wb,
      sheet = sheet_name,
      x = "Normality | Shapiro-Wilk",
      startCol = start_col,
      startRow = start_row + 2 + var_row + 4
    )
    openxlsx::writeData(
      wb = wb,
      sheet = sheet_name,
      x = norm,
      startCol = start_col,
      startRow = start_row + 3 + var_row + 4
    )
    
  }
  
  
  # -- WITHIN-SUBJECTS -- #
  if (isTRUE(within)) {
    
    # Sphericity Test
    openxlsx::writeData(
      wb = wb,
      sheet = sheet_name,
      x = "Sphericity | Mauchly's Test",
      startCol = start_col,
      startRow = start_row + 2 + var_row + 4
    )
    openxlsx::writeData(
      wb = wb,
      sheet = sheet_name,
      x = sph,
      startCol = start_col,
      startRow = start_row + 3 + var_row + 4
    )
    
  }
  
  
  
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
  apply_anova_var_format(
    wb = wb,
    sheet = sheet_name,
    mod_row = var_row,
    mod_col = var_col,
    start_col = start_col,
    start_row = start_row + 2,
    digits = digits
  )
  
  # Apply format to normality / sphericity assumption
  apply_anova_norm_sph_format(
    wb = wb,
    sheet = sheet_name,
    mod_row = nsp_row,
    mod_col = nsp_col,
    start_col = start_col,
    start_row = start_row + 2 + var_row + 4,
    sph = if (isTRUE(within)) {TRUE} else {FALSE}
  )
  
  
  # Expand column width of cols B & C
  openxlsx::setColWidths(wb,
                         sheet = sheet_name,
                         cols = "B",
                         widths = 20)
  
  # Hide gridlines
  openxlsx::showGridLines(wb,
                          sheet = sheet_name,
                          showGridLines = FALSE)
  
}
