#' Add ANOVA Summary Sheet
#'
#' @param wb An \code{openxlsx} workbook object to add the sheet to.
#' @param sheet_name A character string reflecting the name of the worksheet.
#' @param anova_object The ANOVA object from \code{anova_wrapper}.
#' @param digits A numeric value indicating the number of digits to round to.
#' @param name An optional character string reflecting the name of the study.
#'
#' @export
add_anova_sum <- function(
    wb,
    sheet_name,
    anova_object,
    name = NULL,
    digits = 3,
    summary_type = 1
) {
  
  
  # PARAMETERS --------------------------------------------------------------
  
  # Create worksheet
  openxlsx::addWorksheet(
    wb,
    sheetName = sheet_name
  )
  
  # Rename function
  rename_anova <- function(sum) {
    
    sum <- dplyr::rename(
      sum,
      Effect = name,
      "Sum Sq" = ss,
      "Mean Sq" = ms,
      "\u03B7\u00B2" = etaSq,
      "\u03B7\u00B2\u209A" = etaSqP,
      "Cohen's f" = cohen_f
    ) %>% 
      
      # Substitute ":" with multiplication sign
      dplyr::mutate(
        Effect = gsub(":", " \u00D7 ", Effect)
      )
    
    if ("ges" %in% names(sum)) {
      
      sum <- dplyr::rename(
        sum,
        "G-\u03B7\u00B2" = ges
      )
      
    }
    
    if ("omegaSq" %in% names(sum)) {
      
      sum <- dplyr::rename(
        sum,
        "\u03C9\u00B2" = omegaSq
      )
      
    }
    
    return(sum)
    
  }
  
  # Type
  type <- anova_object[["meta_data"]][["anova_type"]]
  interaction <- !is.null(anova_object[["meta_data"]][["interaction"]])
  
  # Type logic
  between <- type %in% c("One-way", "Factorial")
  within <- type %in% c("Repeated Measures", "Mixed Model")
  
  
  # Revert summary type to 1 if only between subjects ANOVA
  if (isTRUE(between) && summary_type == 2) {
    
    summary_type = 1
    
  }
  
  
  # Starting column
  start_col = 2
  
  # Starting row
  start_row = 3
  
  
  
  # ANOVA SUMMARY -----------------------------------------------------------
  
  # If Summary Type is 1, combine BS and WS summaries together
  if (summary_type == 1) {
    
    if (isTRUE(within)) {
      
      # Get both direct & indirect pathway effects
      sum_bs <- anova_object[["bs_summary"]]
      sum_ws <- anova_object[["ws_summary"]]
      
      
      # Get nrow lengths for formatting later
      
      # Between-subjects summary parameters
      bssum_row <- nrow(sum_bs)
      bssum_col <- ncol(sum_bs)
      
      # Within-subjects summary parameters
      wssum_row <- nrow(sum_ws)
      wssum_col <- ncol(sum_ws)
      
      
      # Grab column names
      eff_cols <- colnames(sum_ws)
      
      # Create effect type labels
      eff_types <- data.frame(
        matrix(
          ncol = length(eff_cols),
          nrow = 2
        )
      )
      
      
      # Rename the effect type columns
      colnames(eff_types) <- eff_cols
      
      # Fill in effect types
      eff_types[1, 1] <- "Within Subjects Effects"
      eff_types[2, 1] <- "Between Subjects Effects"
      
      # Remove all other information
      eff_types[1,-1] <- NA
      eff_types[2,-1] <- NA
      
      # Create effects dataframe
      sum <- rbind(
        eff_types[1,],
        sum_ws,
        NA,
        eff_types[2,],
        sum_bs
      )
      
      # Apply renamer
      sum_bs <- rename_anova(
        sum
      )
      
      # Between-subjects summary parameters
      bssum_row <- nrow(sum_bs)
      bssum_col <- ncol(sum_bs)
      
      
    } else {
      
      # Get direct pathway effects
      sum_bs <- rename_anova(
        anova_object[["bs_summary"]]
      )
      
      # Between-subjects summary parameters
      bssum_row <- nrow(sum_bs)
      bssum_col <- ncol(sum_bs)
      
    }
    
  }

  
  # If Summary Type is 2, keep BS and WS summaries separate
  if (summary_type == 2) {
    
    # Rename between-subjects summary
    sum_bs <- rename_anova(
      anova_object[["bs_summary"]]
    )
    
    # Rename within-subjects summary if ANOVA is RM or Mixed Model
    sum_ws <- if (isTRUE(within)) {
      
      rename_anova(
        anova_object[["ws_summary"]]
      )
      
    } else {
      
      NULL
      
    }
    
    # Between-subjects summary parameters
    bssum_row <- nrow(sum_bs)
    bssum_col <- ncol(sum_bs)
    
    # Within-subjects summary parameters
    wssum_row <- nrow(sum_ws)
    wssum_col <- ncol(sum_ws)
    
  }
  
  
  
  # WRITE DATA --------------------------------------------------------------
  
  # -- SUMMARY & TITLE -- #
  openxlsx::writeData(
    wb = wb,
    sheet = sheet_name,
    x = if(!is.null(name)) {
      paste0(name,
             " - ",
             type,
             " ANOVA")
    } else {
      paste0(type,
             " ANOVA")
    },
    startCol = start_col,
    startRow = start_row
  )
  
  
  # -- SUMMARY TYPE 1 -- #
  if (summary_type == 1) {
    
    # -- SUMMARY -- #
    openxlsx::writeData(
      wb = wb,
      sheet = sheet_name,
      x = "Model Effects",
      startCol = start_col,
      startRow = start_row + 2
    )
    openxlsx::writeData(
      wb = wb,
      sheet = sheet_name,
      x = sum_bs,
      startCol = start_col,
      startRow = start_row + 3
    )
    
  }
  
  
  # -- SUMMARY TYPE 2 -- #
  if (summary_type == 2) {
    
    # -- BETWEEN-SUBJECTS -- #
    openxlsx::writeData(
      wb = wb,
      sheet = sheet_name,
      x = dplyr::case_when(
        isTRUE(between) ~ "Model Effects",
        isTRUE(within)  ~ "Between-Subjects Model Effects"
      ),
      startCol = start_col,
      startRow = start_row + 2
    )
    openxlsx::writeData(
      wb = wb,
      sheet = sheet_name,
      x = sum_bs,
      startCol = start_col,
      startRow = start_row + 3
    )
    
    
    # If summary type 2, write separate WS summary
    if (isTRUE(within)) {
      
      # -- WITHIN-SUBJECTS -- #
      
      openxlsx::writeData(
        wb = wb,
        sheet = sheet_name,
        x = "Within-Subjects Model Effects",
        startCol = start_col,
        startRow = start_row + 2 + bssum_row + 4
      )
      openxlsx::writeData(
        wb = wb,
        sheet = sheet_name,
        x = sum_ws,
        startCol = start_col,
        startRow = start_row + 3 + bssum_row + 4
      )
      
    }
    
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
  
  # Apply summary to first summary
  apply_anova_summary_formatter(
    wb = wb,
    sheet = sheet_name,
    mod_row = bssum_row,
    mod_col = bssum_col,
    start_col = start_col,
    start_row = start_row + 2,
    digits = digits,
    correction = dplyr::case_when(
      summary_type == 2 & isTRUE(within) ~ FALSE,
      isTRUE(between) ~ FALSE,
      .default = TRUE
    ),
    bs_row = if(summary_type == 1 && isTRUE(within)) {bssum_row} else {NULL},
    ws_row = if(summary_type == 1 && isTRUE(within)) {wssum_row} else {NULL}
  )
  
  # If both within and between, then apply to second summary
  if (summary_type == 2) {
    
    apply_anova_summary_formatter(
      wb = wb,
      sheet = sheet_name,
      mod_row = wssum_row,
      mod_col = wssum_col,
      start_col = start_col,
      start_row = start_row + 2 + bssum_row + 4,
      digits = digits,
      correction = if (anova_object[["meta_data"]][["correction"]] == "none") {FALSE} else {TRUE}
    )
    
  }
  
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
