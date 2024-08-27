#' Title
#'
#' @param wb 
#' @param plssem_mod 
#' @param sheet_name 
#' @param digits 
#' 
#' @export
add_plssem_wide_table <- function(
    
    wb,
    plssem_mod,
    sheet_name,
    digits = 3 
    
) {
  
  # Outcome pathways
  paths <- plssem_mod[["Meta_Data"]][["paths"]]
  
  # Get table
  table <- plssem_mod[["Tables_Wide"]]
  
  # Get table names
  names(table) <- c("Variable",
                    rep(c("\u03B2",
                          "Lower",
                          "Upper",
                          "p"),
                        length(paths)))
  

  
  # Create the new vector with NAs interspersed
  mod_names <- sprintf("Model %01d", seq(length(paths)))
  
  # Define rows and columns
  mod_row <- nrow(table)
  mod_col <- ncol(table)
  
  # Specify table title
  title_table <- "PLS-SEM Direct Effects Table - Wide Format"
  
  # Add model worksheet
  openxlsx::addWorksheet(wb,
                         sheet_name)
  
  # Starting column
  start_col <- 2
  
  # Starting row
  start_row <- 3
  
  
  # MERGE CELLS -------------------------------------------------------------
  
  # Create function to merge model names
  merge_cells <- function(wb,
                          sheet,
                          row,
                          n,
                          beg_col,
                          merge_n) {
    for(i in 0:(n-1)) {
      startCol <- beg_col + i*4
      endCol <- startCol + merge_n
      openxlsx::mergeCells(wb,
                           sheet,
                           rows = row,
                           cols = startCol:endCol)
    }
  }
  
  merge_cells(wb,
              sheet_name,
              row = start_row + 1,
              n = length(paths),
              beg_col = 3,
              merge_n = 3)
  merge_cells(wb,
              sheet_name,
              row = start_row + 2,
              n = length(paths),
              beg_col = 3,
              merge_n = 3)
  merge_cells(wb,
              sheet_name,
              row = start_row + 3,
              n = length(paths),
              beg_col = 4,
              merge_n = 1)
  merge_cells(wb,
              sheet_name,
              row = start_row + 1 + mod_row,
              n = length(paths),
              beg_col = 3,
              merge_n = 3)
  merge_cells(wb,
              sheet_name,
              row = start_row + 1 + mod_row + 1,
              n = length(paths),
              beg_col = 3,
              merge_n = 3)
  merge_cells(wb,
              sheet_name,
              row = start_row + 1 + mod_row + 2,
              n = length(paths),
              beg_col = 3,
              merge_n = 3)
  merge_cells(wb,
              sheet_name,
              row = start_row + 1 + mod_row + 3,
              n = length(paths),
              beg_col = 3,
              merge_n = 3)
  
  
  # WRITE DATA --------------------------------------------------------------
  
  write_modname <- function(wb,
                            sheet,
                            row,
                            names,
                            beg_col){
    
    for(i in 0:(length(names)-1)) {
      name = names[[i+1]]
      startCol <- beg_col + i*4
      openxlsx::writeData(wb,
                          sheet,
                          x = name,
                          startCol = startCol,
                          startRow = row)
    }
    
  }
  
  write_modname(wb,
                sheet_name,
                row = 4,
                names = mod_names,
                beg_col = 3)
  write_modname(wb,
                sheet_name,
                row = 5,
                names = paths,
                beg_col = 3)
  write_modname(wb,
                sheet_name,
                row = 6,
                names = rep("95% CI", length(paths)),
                beg_col = 4)
  
  # Write title
  openxlsx::writeData(wb,
                      sheet = sheet_name,
                      x = "PLS-SEM Direct Effects Table - Wide Format",
                      startCol = start_col,
                      startRow = start_row)
  
  # Write main table
  openxlsx::writeData(wb,
                      sheet = sheet_name,
                      x = table,
                      startCol = start_col,
                      startRow = start_row + 4)
  
  # Write note
  openxlsx::writeData(wb,
                      sheet = sheet_name,
                      x = "NOTE: All effects are bootstrapped.",
                      startCol = start_col,
                      startRow = start_row + 4 + mod_row + 1)
  
  # Apply formatter
  apply_wide_table_formatter(
    wb,
    sheet = sheet_name,
    df = table,
    paths = paths,
    start_col = start_col,
    start_row = start_row,
    digits = 3
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