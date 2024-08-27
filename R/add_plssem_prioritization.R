#' Add PLS-SEM Prioritization Workbook Sheet
#'
#' @param wb 
#' @param plssem_mod 
#' @param sheet_name 
#' @param digits 
#'
#' @export
add_plssem_prioritization <- function(
    
  wb,
  plssem_mod,
  sheet_name,
  digits = 3
  
) {
  
  # Add model worksheet
  openxlsx::addWorksheet(wb,
                         sheet_name)
  
  # Get outcomes
  outcomes <- plssem_mod[["meta_data"]][["outcomes"]]
  
  # Get total effects
  tot_table <- plssem_mod[["Total_Effects"]] %>% 
    
    # Separate predictor from outcome
    tidyr::separate(
      path,
      into = c("predictor", "outcome"),
      sep = "  ->  "
    ) %>% 
    
    # Filter to just outcomes
    dplyr::filter(
      outcome %in% outcomes
    ) %>% 
    
    # Select relevant variables
    dplyr::select(
      predictor,
      outcome,
      boot_est
    ) %>% 
    
    # Pivot into working table
    tidyr::pivot_wider(
      names_from = outcome,
      values_from = boot_est
    ) %>% 
    
    # Reorder to descending first outcome
    dplyr::arrange(
      dplyr::desc(!!sym(colnames(.)[2]))
    ) %>% 
    
    # Rename
    dplyr::rename(
      Predictor = predictor
    )
  
  
  # Get priority effects
  prio_table <- plssem_mod[["Prioritization"]] %>% 
    
    # Rename
    dplyr::rename(
      Indicator = indicator
    ) %>% 
    
    # Recode
    dplyr::mutate(
      across(.cols = colnames(.)[-1],
             \(x) x * 100)
    )
  
  
  # Get weights
  wgt_table <- plssem_mod[["Weights_Table"]] %>% 
    
    # Normalize
    dplyr::mutate(
      dplyr::across(
        .cols = -1,
        \(x) x / sum(x,
                     na.rm = T)
      )
    ) %>% 
    
    # Rename
    dplyr::rename(
      Variable = variable
    ) %>% 
    
    # Remove certain columns
    dplyr::select(
      -c(dplyr::all_of(outcomes))
    ) %>% 
    
    # Remove outcome indicators
    dplyr::filter(
      !rowSums(is.na(.[-1])) == (ncol(.)-1)
    )
  
  
  # Rearrange weights table to descending
  wgt_names <- rev(colnames(wgt_table[-1]))
  
  for (i in wgt_names) {
    
    wgt_table <- wgt_table %>% 
      dplyr::arrange(
        dplyr::desc(!!sym(i))
      )
    
  }
  
  
  # Establish Start Column and Row
  start_col <- 2
  start_row <- 3
  
  # Table rows and columns
  tot_row <- nrow(tot_table)
  tot_col <- ncol(tot_table)
  
  prio_row <- nrow(prio_table)
  prio_col <- ncol(prio_table)
  
  wgt_row <- nrow(wgt_table)
  wgt_col <- ncol(wgt_table)
  
  
  # MERGE CELLS -------------------------------------------------------------
  
  # Prioritization Title merge cells
  openxlsx::mergeCells(
    wb,
    sheet = sheet_name,
    cols = start_col:(start_col+1),
    rows = (start_row + 1 + tot_row + 3):(start_row + 1 + tot_row + 4)
  )
  
  # WRITE DATA --------------------------------------------------------------
  
  # -- TOTAL EFFECTS -- #
  
  # Write title for Total Effects
  openxlsx::writeData(
    wb,
    sheet = sheet_name,
    x = "Total Effects",
    startCol = start_col,
    startRow = start_row
  )
  
  # Write Total Effects table
  openxlsx::writeDataTable(
    wb,
    sheet = sheet_name,
    x = tot_table,
    startCol = start_col,
    startRow = start_row + 1,
    tableStyle = "TableStyleMedium2"
  )
  
  # Write Total Effects note
  openxlsx::writeData(
    wb,
    sheet = sheet_name,
    x = "Note: Values closer to 1 = stronger effect.",
    startCol = start_col,
    startRow = start_row + 1 + tot_row + 1
  )
  
  
  # -- PRIORITIZATION -- #
  
  # Write title for Prioritization
  openxlsx::writeData(
    wb,
    sheet = sheet_name,
    x = "Prioritization Analysis",
    startCol = start_col,
    startRow = start_row + 1 + tot_row + 3
  )
  
  # Write Prioritization table
  openxlsx::writeDataTable(
    wb,
    sheet = sheet_name,
    x = prio_table,
    startCol = start_col,
    startRow = start_row + 1 + tot_row + 5,
    tableStyle = "TableStyleMedium2"
  )
  
  # Write Prioritization note
  openxlsx::writeData(
    wb,
    sheet = sheet_name,
    x = "Note: > 100 = Above average; < 100 = Below Average",
    startCol = start_col,
    startRow = start_row + 1 + tot_row + 5 + prio_row + 1
  )
  
  
  
  # -- NORMALIZED WEIGHTS -- #
  
  # Write title for Normalized Weights
  openxlsx::writeData(
    wb,
    sheet = sheet_name,
    x = "Indicator Weights",
    startCol = start_col + tot_col + 1,
    startRow = start_row
  )
  
  # Write Normalized Weights table
  openxlsx::writeDataTable(
    wb,
    sheet = sheet_name,
    x = wgt_table,
    startCol = start_col + tot_col + 1,
    startRow = start_row + 1,
    tableStyle = "TableStyleMedium2"
  )
  
  # Add note
  openxlsx::writeData(
    wb,
    sheet = sheet_name,
    x = "NOTE: Weights are normalized to sum to 1.",
    startCol = start_col + tot_col + 1,
    startRow = start_row + wgt_row + 2
  )
  
  
  # APPLY FORMATTING --------------------------------------------------------
  
  # -- CREATE STYLES -- #
  
  # Title Style
  title_style <- openxlsx::createStyle(
    fontSize = 16,
    textDecoration = "bold"
  )
  
  # Header Centering Style
  header_center_style <- openxlsx::createStyle(
    halign = "center"
  )
  
  # Body Styles
  body_pct_style <- openxlsx::createStyle(
    numFmt = "0%"
  )
  body_idx_style <- openxlsx::createStyle(
    numFmt = "0",
    halign = "center"
  )
  body_rnd_style <- openxlsx::createStyle(
    numFmt = "0.000",
    halign = "center"
  )
  
  
  # -- APPLY FORMATTING -- #
  
  # Titles
  openxlsx::addStyle(
    wb,
    sheet = sheet_name,
    style = title_style,
    cols = start_col,
    rows = start_row
  )
  openxlsx::addStyle(
    wb,
    sheet = sheet_name,
    style = title_style,
    cols = start_col,
    rows = start_row + 1 + tot_row + 3
  )
  openxlsx::addStyle(
    wb,
    sheet = sheet_name,
    style = title_style,
    cols = start_col + tot_col + 1,
    rows = start_row
  )
  
  # Headers
  openxlsx::addStyle(wb,
    sheet = sheet_name,
    style = header_center_style,
    cols = (start_col + 1):(start_col + length(outcomes)),
    rows = start_row + 1
  )
  openxlsx::addStyle(
    wb,
    sheet = sheet_name,
    style = header_center_style,
    cols = (start_col + 1):(start_col + length(outcomes)),
    rows = start_row + 1 + tot_row + 3 + 2
  )
  openxlsx::addStyle(
    wb,
    sheet = sheet_name,
    style = header_center_style,
    cols = (start_col + tot_col + 2):(start_col + tot_col + 1 + wgt_col),
    rows = start_row + 1
  )
  
  # Bodies
  openxlsx::addStyle(
    wb,
    sheet = sheet_name,
    style = body_rnd_style,
    gridExpand = TRUE,
    cols = (start_col + 1):(start_col + length(outcomes)),
    rows = (start_row + 2):(start_row + tot_row + 1)
  )
  openxlsx::addStyle(
    wb,
    sheet = sheet_name,
    style = body_idx_style,
    gridExpand = TRUE,
    cols = (start_col + 1):(start_col + length(outcomes)),
    rows = (start_row + 1 + tot_row + 6):(start_row + tot_row + 6 + prio_row)
  )
  openxlsx::addStyle(
    wb,
    sheet = sheet_name,
    style = body_pct_style,
    gridExpand = TRUE,
    cols = (start_col + tot_col + 2):(start_col + tot_col + 1 + wgt_col),
    rows = (start_row + 2):(start_row + 2 + wgt_row)
  )
  
  
  # CONDITIONAL FORMATTING --------------------------------------------------
  
  # Databar conditional formatting
  openxlsx::conditionalFormatting(
    wb,
    sheet = sheet_name,
    cols = (start_col + tot_col + 2):(start_col + tot_col + 1 + wgt_col),
    rows = (start_row + 2):(start_row + 2 + wgt_row),
    type = "databar",
    style = "#638EC6",
    gradient = FALSE
  )
  
  
  # MISC --------------------------------------------------------------------
  
  # Expand column width of col B
  openxlsx::setColWidths(
    wb,
    sheet = sheet_name,
    cols = "B",
    widths = 20
  )
  
  # Hide gridlines
  openxlsx::showGridLines(
    wb,
    sheet = sheet_name,
    showGridLines = FALSE
  )
  
}