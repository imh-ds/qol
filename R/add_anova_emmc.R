#' Add ANOVA Estimated Marginal Means Contrasts Sheet
#'
#' @param wb An \code{openxlsx} workbook object to add the sheet to.
#' @param sheet_name A character string reflecting the name of the worksheet.
#' @param anova_object The ANOVA object from \code{anova_wrapper}.
#' @param digits A numeric value indicating the number of digits to round to.
#' @param name An optional character string reflecting the name of the study.
#'
#' @export
add_anova_emmc <- function(
    wb,
    sheet_name,
    anova_object,
    digits = 3,
    name = NULL
) {
  
  # PARAMETERS --------------------------------------------------------------

  # Rounding parameter
  rnd <- paste0(
    '%.',
    digits,
    'f'
  )
  
  # Create worksheet
  openxlsx::addWorksheet(
    wb,
    sheetName = sheet_name
  )
  
  # Starting column
  start_col = 2
  
  # Starting row
  start_row = 3
  
  
  # ESTIMATED MARGINAL MEANS CONTRAST ---------------------------------------
  
  emm <- anova_object[["emmc"]]
  
  # If p-value adjustment exists, apply renaming
  if ("pnone" %in% names(emm)) {
    emm <- dplyr::rename(emm,
                         p = pnone)
  }
  
  if ("ptukey" %in% names(emm)) {
    emm <- dplyr::rename(emm,
                         p = ptukey)
  }
  
  if ("pscheffe" %in% names(emm)) {
    emm <- dplyr::rename(emm,
                         p = pscheffe)
  }
  
  if ("pholm" %in% names(emm)) {
    emm <- dplyr::rename(emm,
                         p = pholm)
  }
  
  if ("pbonf" %in% names(emm)) {
    emm <- dplyr::rename(emm,
                         p = pbonf)
  }
  
  # Effect size type
  es_name <- dplyr::case_when(
    anova_object[["meta_data"]][["es_type"]] == "hedges" ~ "g",
    anova_object[["meta_data"]][["es_type"]] == "cohen" ~ "d",
    anova_object[["meta_data"]][["es_type"]] == "glass" ~ "\u0394"
  )
  
  es_se_name <- paste0(es_name,
                       " SE")
  
  
  # If between-subjects ANOVA, create paired variable
  emm <- if (anova_object[["meta_data"]][["anova_type"]] %in% c("One-way", "Factorial")) {
    
    emm %>% 
      dplyr::mutate(
        paired = ifelse(is.na(effect), NA, "Independent")
      ) %>% 
      dplyr::relocate(
        paired,
        .before = es_type
      )
    
  } else {
    
    emm %>% 
      dplyr::mutate(
        paired = ifelse(paired, "Paired", "Independent")
      )
    
  }
  
  
  # Apply transformations
  emm <- emm %>% 
    
    # Recode values
    dplyr::mutate(
      `In Text` = ifelse(
        is.na(effect),
        NA,
        paste0(
          "(",
          group_1,
          " - ",
          group_2,
          ", M diff = ",
          sprintf(rnd, md),
          ", SE diff = ",
          sprintf(rnd, se),
          ", t = ",
          sprintf(rnd, t),
          ", p ",
          ifelse(p < .001, "< .001",
                 paste0("= ", sprintf(rnd, p))),
          ", ",
          dplyr::case_when(
            anova_object[["meta_data"]][["es_type"]] == "hedges" ~ "g = ",
            anova_object[["meta_data"]][["es_type"]] == "cohen" ~ "d = ",
            anova_object[["meta_data"]][["es_type"]] == "glass" ~ "\u0394 = "
          ),
          sprintf(rnd, es),
          ")"
        )
      ),
      effect = ifelse(!duplicated(effect), gsub("\\*", " \u00D7 ", effect), NA)
    ) %>% 
  
  # Rename
  dplyr::rename(
    Effect = effect,
    "Group 1" = group_1,
    "Group 2" = group_2,
    "M Dif" = md,
    "SE Dif" = se,
    Type = paired,
    "Effect Size" = es_type,
    !!sym(es_name) := es,
    !!sym(es_se_name) := es_se,
    Lower = es_ci_lower,
    Upper = es_ci_upper
  )
  
  
  # Separate the in-text report as a separate data frame
  emm_intext <- emm %>% 
    dplyr::select(
      `In Text`
    )
  
  emm <- emm %>% 
    dplyr::select(
      -`In Text`
    )
  
  
  
  # EMM PARAMETERS ----------------------------------------------------------
  
  # EMM Contrast parameters
  emm_row <- nrow(emm)
  emm_col <- ncol(emm)
  
  
  # WRITE DATA --------------------------------------------------------------
  
  # -- SUMMARY & TITLE -- #
  openxlsx::writeData(
    wb = wb,
    sheet = sheet_name,
    x = if(!is.null(name)) {
      paste0(name,
             " - Estimated Marginal Means Contrasts")
    } else {
      "Estimated Marginal Means Contrasts"
    },
    startCol = start_col,
    startRow = start_row
  )
  
  # -- EMM PAIRWISE COMPARISONS -- #
  
  # Merge confidence interval cells
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
  
  # Merge group 1 statistics
  openxlsx::mergeCells(
    wb,
    sheet = sheet_name,
    cols = (start_col + emm_col - 12):(start_col + emm_col - 10),
    rows = start_row + 3
  )
  openxlsx::writeData(
    wb,
    sheet = sheet_name,
    x = "Group 1",
    startCol = start_col + emm_col - 12,
    startRow = start_row + 3
  )
  
  # Merge group 2 statistics
  openxlsx::mergeCells(
    wb,
    sheet = sheet_name,
    cols = (start_col + emm_col - 9):(start_col + emm_col - 7),
    rows = start_row + 3
  )
  openxlsx::writeData(
    wb,
    sheet = sheet_name,
    x = "Group 2",
    startCol = start_col + emm_col - 9,
    startRow = start_row + 3
  )
  
  
  # Title
  openxlsx::writeData(
    wb = wb,
    sheet = sheet_name,
    x = "Pairwise Comparisons",
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
  
  
  # Fix column names for groups 1 and 2
  openxlsx::writeData(
    wb = wb,
    sheet = sheet_name,
    x = "Mean",
    startCol = start_col + emm_col - 12,
    startRow = start_row + 4
  )
  openxlsx::writeData(
    wb = wb,
    sheet = sheet_name,
    x = "SE",
    startCol = start_col + emm_col - 11,
    startRow = start_row + 4
  )
  openxlsx::writeData(
    wb = wb,
    sheet = sheet_name,
    x = "n",
    startCol = start_col + emm_col - 10,
    startRow = start_row + 4
  )
  
  
  openxlsx::writeData(
    wb = wb,
    sheet = sheet_name,
    x = "Mean",
    startCol = start_col + emm_col - 9,
    startRow = start_row + 4
  )
  openxlsx::writeData(
    wb = wb,
    sheet = sheet_name,
    x = "SE",
    startCol = start_col + emm_col - 8,
    startRow = start_row + 4
  )
  openxlsx::writeData(
    wb = wb,
    sheet = sheet_name,
    x = "n",
    startCol = start_col + emm_col - 7,
    startRow = start_row + 4
  )
  
  
  # -- EMM PAIRWISE COMPARISON TEXTS -- #
  openxlsx::writeData(
    wb = wb,
    sheet = sheet_name,
    x = "Pairwise Comparisons Text",
    startCol = (start_col + emm_col + 1),
    startRow = start_row + 2
  )
  openxlsx::writeData(
    wb,
    sheet = sheet_name,
    x = emm_intext,
    startCol = (start_col + emm_col + 1),
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
  apply_anova_emmc_format(
    wb = wb,
    sheet = sheet_name,
    mod_row = emm_row,
    mod_col = emm_col,
    start_col = start_col,
    start_row = start_row + 2,
    digits = digits
  )
  
  # Apply text format
  apply_text_format(
    wb = wb,
    sheet = sheet_name,
    df = emm_intext,
    start_col = (start_col + 1 + emm_col),
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