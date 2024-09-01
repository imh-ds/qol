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
  name = NULL,
  digits = 3 
) {
  
  # MODEL PARAMETERS --------------------------------------------------------
  
  # Set parameter for rounding
  rnd <- paste0('%.',
                digits,
                "f")
  
  paths_tab <- plssem_mod[["Direct_Effects"]]
  paths <- plssem_mod[["meta_data"]][["paths"]]
  pls_model <- plssem_mod[["PLS_Model"]]
  
  reg_list_wide <- lapply(
    paths,
    function(var) {
      
      pt_outcome <- paths_tab %>%
        dplyr::filter(grepl(paste0(var,"$"),
                            path))
      
      pt_est <- pt_outcome %>% 
        dplyr::select(path,
                      boot_est,
                      lower_ci,
                      upper_ci,
                      p)
      
      # Clean up labels of variables
      pt_reg <- pt_est %>% 
        dplyr::mutate(path = gsub("  ->.*", "", path))
      
      # Get basic model descriptives and metrics
      pt_desc <- data.frame(
        "path" = c("Observations",
                   "R\u00B2",
                   "R\u00B2 Adj"),
        "boot_est" = c(nrow(pls_model[["construct_scores"]]),
                       as.data.frame(pls_model[["rSquared"]])[[var]]),
        "lower_ci" = rep(NA,3),
        "upper_ci" = rep(NA,3),
        "p" = rep(NA,3)
      )
      
      # Get degrees of freedom for number of predictors (numerator)
      degfree1 <- length(pt_est[["path"]])
      # Get degrees of freedom for number of observations minus predictors - 1 (denominator)
      degfree2 <- nrow(pls_model[["construct_scores"]]) - (1 + degfree1)
      
      # Get R^2 value for F-statistic calculation
      rsqr <- as.data.frame(pls_model[["rSquared"]])[[var]][1]
      
      # Calculate F-statistic
      pt_fstat <- data.frame(
        "path" = c("F"),
        "boot_est" = (rsqr / (1 - rsqr)) * (degfree2 / degfree1),
        "lower_ci" = NA,
        "upper_ci" = NA,
        "p" = NA
      )
      
      # Create table
      pt_table <- rbind(pt_reg,
                        pt_desc,
                        pt_fstat) %>%
        dplyr::rename("variable" = path,
                      !!sym(paste0(var," est")) := boot_est,
                      !!sym(paste0(var," lower")) := lower_ci,
                      !!sym(paste0(var," upper")) := upper_ci,
                      !!sym(paste0(var," p")) := p)
      
      # Return
      return(pt_table)
      
    }
  )
  
  # Collapse the path table list into one dataframe and send R^2 to the bottom
  reg_tab_wideeff <- suppressMessages(
    
    purrr::reduce(reg_list_wide,
                  dplyr::full_join)
    
  ) %>%
    dplyr::filter(
      !grepl("R\\\u00B2|^Observations$|^F$",
             variable)
    )
  
  reg_tab_widedesc <- suppressMessages(
    purrr::reduce(reg_list_wide,
                  dplyr::full_join)
  ) %>%
    dplyr::filter(
      grepl("R\\\u00B2|^Observations$|^F$",
            variable)
    )
  
  # Combine into final product
  reg_wide_table <- rbind(reg_tab_wideeff,
                          reg_tab_widedesc)
  

  # TABLE PARAMETERS --------------------------------------------------------

  # Get table
  table <- reg_wide_table
  
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
  title_table <- "Direct Effects Table"
  
  # Add model worksheet
  openxlsx::addWorksheet(wb,
                         sheet_name)
  
  # Starting column
  start_col <- 2
  
  # Starting row
  start_row <- 3
  
  
  # TITLE -------------------------------------------------------------------
  
  # -- TITLE -- #
  openxlsx::writeData(
    wb = wb,
    sheet = sheet_name,
    x = if(!is.null(name)) {
      paste0(name,
             " - PLS-SEM Table (Wide Version)")
    } else {
      "PLS-SEM Table (Wide Version)"
    },
    startCol = start_col,
    startRow = start_row
  )
  
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
  
  merge_cells(
    wb,
    sheet_name,
    row = start_row + 3,
    n = length(paths),
    beg_col = 3,
    merge_n = 3)
  merge_cells(
    wb,
    sheet_name,
    row = start_row + 4,
    n = length(paths),
    beg_col = 3,
    merge_n = 3)
  merge_cells(
    wb,
    sheet_name,
    row = start_row + 5,
    n = length(paths),
    beg_col = 4,
    merge_n = 1)
  merge_cells(
    wb,
    sheet_name,
    row = start_row + 3 + mod_row,
    n = length(paths),
    beg_col = 3,
    merge_n = 3)
  merge_cells(
    wb,
    sheet_name,
    row = start_row + 3 + mod_row + 1,
    n = length(paths),
    beg_col = 3,
    merge_n = 3)
  merge_cells(
    wb,
    sheet_name,
    row = start_row + 3 + mod_row + 2,
    n = length(paths),
    beg_col = 3,
    merge_n = 3)
  merge_cells(
    wb,
    sheet_name,
    row = start_row + 3 + mod_row + 3,
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
                row = 6,
                names = mod_names,
                beg_col = 3)
  write_modname(wb,
                sheet_name,
                row = 7,
                names = paths,
                beg_col = 3)
  write_modname(wb,
                sheet_name,
                row = 8,
                names = rep("95% CI", length(paths)),
                beg_col = 4)
  
  # Write title
  openxlsx::writeData(wb,
                      sheet = sheet_name,
                      x = "Direct Effects Table",
                      startCol = start_col,
                      startRow = start_row + 2)
  
  # Write main table
  openxlsx::writeData(wb,
                      sheet = sheet_name,
                      x = table,
                      startCol = start_col,
                      startRow = start_row + 6)
  
  # Write note
  openxlsx::writeData(wb,
                      sheet = sheet_name,
                      x = "NOTE: All effects are bootstrapped.",
                      startCol = start_col,
                      startRow = start_row + 6 + mod_row + 1)
  
  # Apply formatter
  apply_wide_table_formatter(
    wb,
    sheet = sheet_name,
    df = table,
    paths = paths,
    start_col = start_col,
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