#' Affix PLS-SEM Model Diagnostic Workbook Sheet
#'
#' @param wb 
#' @param sheet_name 
#' @param digits 
#'
#' @return
#' @export
#'
#' @examples
affix_plssem_diagnostic <- function(
    wb,
    plssem_mod,
    sheet_name,
    digits = 3) {
  
  # Grab data frames
  model_e <- plssem_mod[["ModelR"]]         # Model Coefficients
  model_f <- plssem_mod[["ModelES"]]        # Model Effect Sizes
  model_v <- plssem_mod[["VIF"]]            # Model VIF
  model_r <- plssem_mod[["Reliability"]]    # Model Reliability
  model_l <- plssem_mod[["Validity_FL"]]    # Model Validity Test - FL
  model_h <- plssem_mod[["Validity_HTMT"]]  # Model Validity Test - HTMT
  
  # Change column name to Title
  names(model_e) <- stringr::str_to_title(names(model_e))
  names(model_f) <- stringr::str_to_title(names(model_f))
  names(model_v) <- stringr::str_to_title(names(model_v))
  names(model_r) <- c("Variable",
                      "\u03B1",
                      "\u03C1C",
                      "\u03C1A",
                      "AVE",
                      "Max{r\u1D62\u2C7C}",
                      "Max{HTMT\u1D62\u2C7C}",
                      "\u221AAVE",
                      "AVE Test",
                      "HTMT Test")
  names(model_l) <- stringr::str_to_title(names(model_l))
  names(model_h) <- stringr::str_to_title(names(model_h))
  
  
  # Define rows and columns
  mod_e_row <- nrow(model_e)
  mod_e_col <- ncol(model_e)
  
  mod_f_row <- nrow(model_f)
  mod_f_col <- ncol(model_f)
  
  mod_v_row <- nrow(model_v)
  mod_v_col <- ncol(model_v)
  
  mod_r_row <- nrow(model_r)
  mod_r_col <- ncol(model_r)
  
  mod_l_row <- nrow(model_l)
  mod_l_col <- ncol(model_l)
  
  mod_h_row <- nrow(model_h)
  mod_h_col <- ncol(model_h)
  
  
  # Add model performance worksheet
  openxlsx::addWorksheet(wb,
                         sheet_name)
  
  # Starting column
  start_col = 2
  
  # Starting row
  start_row = 3
  
  

  # WRITE MODEL COEFFICIENTS ------------------------------------------------
  
  openxlsx::writeData(wb,
                      sheet = sheet_name,
                      x = "Model Coefficients",
                      startCol = start_col,
                      startRow = start_row)
  openxlsx::writeData(wb,
                      sheet = sheet_name,
                      x = model_e,
                      startCol = start_col,
                      startRow = start_row + 1)
  openxlsx::writeData(wb,
                      sheet = sheet_name,
                      x = "NOTE: Coefficients are standardized \u03B2.",
                      startCol = start_col,
                      startRow = start_row + 1 + mod_e_row + 1)
  
  # Apply formatter
  wb <- apply_coef_formatter(
    wb = wb,
    sheet = sheet_name,
    df = model_e,
    start_col = start_col,
    start_row = start_row,
    digits = digits
  )
  
  

  # WRITE MODEL VIF ---------------------------------------------------------
  
  openxlsx::writeData(wb,
                      sheet = sheet_name,
                      x = "Model Variation Inflation Factor (VIF)",
                      startCol = start_col,
                      startRow = start_row + 4 + mod_e_row)
  openxlsx::writeData(wb,
                      sheet = sheet_name,
                      x = model_v,
                      startCol = start_col,
                      startRow = start_row + 5 + mod_e_row)
  openxlsx::writeData(wb,
                      sheet = sheet_name,
                      x = "NOTE: VIF < 5 indicates low multicollinearity. VIF > 10 indicates strong multicollinearity.",
                      startCol = start_col,
                      startRow = start_row + 5 + mod_e_row + mod_v_row + 1)
  
  # Apply formatter
  wb <- apply_perf_formatter(
    wb = wb,
    sheet = sheet_name,
    df = model_v,
    start_col = start_col,
    start_row = start_row + 4 + mod_e_row,
    digits = digits
  )
  

  
  # WRITE MODEL EFFECT SIZES ------------------------------------------------
  
  openxlsx::writeData(wb,
                      sheet = sheet_name,
                      x = "Model f\u00B2",
                      startCol = start_col,
                      startRow = start_row + 8 + mod_e_row + mod_v_row)
  openxlsx::writeData(wb,
                      sheet = sheet_name,
                      x = model_f,
                      startCol = start_col,
                      startRow = start_row + 9 + mod_e_row + mod_v_row)
  openxlsx::writeData(wb,
                      sheet = sheet_name,
                      x = "NOTE: f\u00B2 ~ 0.01 = Small Effect, f\u00B2 ~ 0.04 = Medium Effect, f\u00B2 ~ 0.10 = Large Effect.",
                      startCol = start_col,
                      startRow = start_row + 9 + mod_e_row + mod_v_row + mod_f_row + 1)
  
  wb <- apply_perf_formatter(
    wb = wb,
    sheet = sheet_name,
    df = model_f,
    start_col = start_col,
    start_row = start_row + 8 + mod_e_row + mod_v_row,
    digits = digits
  )
  
  

  # WRITE MODEL RELIABILITY & VALIDITY --------------------------------------

  openxlsx::writeData(wb,
                      sheet = sheet_name,
                      x = "Measurement Reliability & Validity",
                      startCol = start_col,
                      startRow = start_row + 12 + mod_e_row + mod_v_row + mod_f_row)
  openxlsx::writeData(wb,
                      sheet = sheet_name,
                      x = model_r,
                      startCol = start_col,
                      startRow = start_row + 13 + mod_e_row + mod_v_row + mod_f_row)
  openxlsx::writeData(wb,
                      sheet = sheet_name,
                      x = "NOTE: \u03B1 = Cronbach's Alpha. \u03C1C = Composite Reliability. \u03C1A = Construct Reliability. AVE = Average Variance Extracted. HTMT = Heterotrait Monotrait Ratio. Discriminant validity established if \u221AAVE > Max{r\u1D62\u2C7C} or Max{HTMT\u1D62\u2C7C} < 0.90.",
                      startCol = start_col,
                      startRow = start_row + 13 + mod_e_row + mod_v_row + mod_f_row + mod_r_row + 1)
  
  # Apply formatter
  wb <- apply_perf_formatter(
    wb = wb,
    sheet = sheet_name,
    df = model_r,
    start_col = start_col,
    start_row = start_row + 12 + mod_e_row + mod_v_row + mod_f_row,
    digits = digits
  )
  
  
  
  # WRITE MODEL VALIDITY TESTS ----------------------------------------------
  
  
  # Fornell-Larcker Criterion Test
  openxlsx::writeData(wb,
                      sheet = sheet_name,
                      x = "Validity Test | Fornell-Larcker Criterion",
                      startCol = start_col,
                      startRow = start_row + 16 + mod_e_row + mod_v_row + mod_f_row + mod_r_row)
  openxlsx::writeData(wb,
                      sheet = sheet_name,
                      x = model_l,
                      startCol = start_col,
                      startRow = start_row + 17 + mod_e_row + mod_v_row + mod_f_row + mod_r_row)
  openxlsx::writeData(wb,
                      sheet = sheet_name,
                      x = "NOTE: AVE = Average Variance Extracted. \u221AAVE given as diagonal values. Bivariate correlations given below the matrix diagonal. Fornell-Larcker Criterion is met if \u221AAVE > Max{r\u1D62\u2C7C}.",
                      startCol = start_col,
                      startRow = start_row + 17 + mod_e_row + mod_v_row + mod_f_row + mod_r_row + mod_l_row + 1)
  
  # Apply formatter
  wb <- apply_perf_formatter(
    wb = wb,
    sheet = sheet_name,
    df = model_l,
    start_col = start_col,
    start_row = start_row + 16 + mod_e_row + mod_v_row + mod_f_row + mod_r_row,
    digits = digits
  )
  
  
  
  # Heterotrait-Monotrait Criterion Test
  openxlsx::writeData(wb,
                      sheet = sheet_name,
                      x = "Validity Test | Heterotrait-Monotrait Ratio of Correlations Criterion",
                      startCol = start_col,
                      startRow = start_row + 20 + mod_e_row + mod_v_row + mod_f_row + mod_r_row + mod_l_row)
  openxlsx::writeData(wb,
                      sheet = sheet_name,
                      x = model_h,
                      startCol = start_col,
                      startRow = start_row + 21 + mod_e_row + mod_v_row + mod_f_row + mod_r_row + mod_l_row)
  openxlsx::writeData(wb,
                      sheet = sheet_name,
                      x = "NOTE: HTMT = Heterotrait Monotrait Ratio. HTMT Criterion met if Max{HTMT\u1D62\u2C7C} < 0.90.",
                      startCol = start_col,
                      startRow = start_row + 21 + mod_e_row + mod_v_row + mod_f_row + mod_r_row + mod_l_row + mod_h_row + 1)
  
  # Apply formatter
  wb <- apply_perf_formatter(
    wb = wb,
    sheet = sheet_name,
    df = model_h,
    start_col = start_col,
    start_row = start_row + 20 + mod_e_row + mod_v_row + mod_f_row + mod_r_row + mod_l_row,
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
  
  # Return
  return(wb)
  
}
