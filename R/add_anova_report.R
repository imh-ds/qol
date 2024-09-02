#' Add ANOVA Reportable Table Format
#'
#' @param wb 
#' @param sheet_name 
#' @param anova_object 
#' @param name 
#' @param digits A numeric value indicating the number of digits to round to.
#' @param report_es A vector of strings indicating which effect sizes to report.
#'   Options include \code{"ges"} for generalized eta-squared, \code{"etaSq"}
#'   for eta-squared, \code{"etaSqP"} for partial eta-squared, \code{"omegaSq"}
#'   for omega-squared, and \code{"cohen_f"} for Cohen's f. Specify \code{NULL}
#'   to not report any effect size.
#'
#' @return
#' @export
#'
#' @examples
add_anova_report <- function(
  wb,
  sheet_name,
  anova_object,
  name = NULL,
  digits = 3,
  report_es = c("etaSqP")
) {
  
  
  # PARAMETERS --------------------------------------------------------------
  
  # Create worksheet
  openxlsx::addWorksheet(
    wb,
    sheetName = sheet_name
  )
  
  # Rounding parameter
  rnd <- paste0(
    '%.',
    digits,
    'f'
  )
  
  # Starting column
  start_col = 2
  
  # Starting row
  start_row = 3
  
  # Effect size length
  es_length <- length(report_es)
  
  # Get correction type
  correction_type <- dplyr::case_when(
    anova_object[["meta_data"]][["correction"]] == "HF" ~ "Huynh-Feldt",
    anova_object[["meta_data"]][["correction"]] == "GG" ~ "Greenhouse-Geisser",
    anova_object[["meta_data"]][["correction"]] == "none" ~ NA,
  )
  
  # Get length of WS & BS vars
  eff_length <- anova_object[["emm"]] %>% 
    
    # Remove standard EMM objects
    dplyr::select(-c(effect,
                     n,
                     mean,
                     se,
                     lower,
                     upper)) %>% 
    
    # Get length of vars
    length()
  
  
  # Grab EMM data frame
  emm <- anova_object[["emm"]] %>% 
    
    # Remove duplicates
    dplyr::mutate(
      dplyr::across(.cols = dplyr::everything(),
                    .fns = function(x) ifelse(!is.na(effect) & is.na(x), 
                                              "-", 
                                              x)),
      effect = ifelse(!duplicated(effect),
                      effect,
                      NA)
    ) %>% 
    
    # Remove mean CI
    dplyr::select(
      -c("lower", "upper")
    )
  
  # If anova object is RM, combine BS and WS summaries
  if (anova_object[["meta_data"]][["anova_type"]] %in% c("Mixed Model",
                                                         "Repeated Measures")) {
    
    # Summaries
    ws_sum <- anova_object[["ws_summary"]]
    bs_sum <- anova_object[["bs_summary"]]
    
    # Merge and prep
    sum <- rbind(ws_sum,
                 bs_sum) %>% 
      
      # Rename name to effect
      dplyr::rename(effect = name) %>% 
      
      # Replace interaction symbol
      dplyr::mutate(effect = gsub(":", "*", effect)) %>% 
      
      # Remove residual information
      dplyr::filter(!effect == "Residual") %>% 
      
      # Select only relevant variables & effect sizes
      dplyr::select(names(.)[1:6],
                    dplyr::all_of(report_es))
    
  }
  
  # Combine into a report table
  report_df <- emm %>% 
    
    # Join with summary
    dplyr::full_join(
      sum,
      by = "effect"
    ) %>% 
    
    # Rename
    dplyr::rename(
      Effect = effect,
      Mean = mean,
      SE = se,
      SS = ss,
      MS = ms
    )
  
  # Rename effect sizes accordingly
  if ("ges" %in% report_es) {
    
    report_df <- dplyr::rename(
      report_df,
      "G-\u03B7\u00B2" = ges
    )
    
  }
  
  if ("etaSq" %in% report_es) {
    
    report_df <- dplyr::rename(
      report_df,
      "\u03B7\u00B2" = etaSq
    )
    
  }
  
  if ("etaSqP" %in% report_es) {
    
    report_df <- dplyr::rename(
      report_df,
      "\u03B7\u00B2\u209A" = etaSqP
    )
    
  }
  
  if ("omegaSq" %in% report_es) {
    
    report_df <- dplyr::rename(
      report_df,
      "\u03C9\u00B2" = omegaSq
    )
    
  }
  
  if ("cohen_f" %in% report_es) {
    
    report_df <- dplyr::rename(
      report_df,
      "Cohen's f" = cohen_f
    )
    
  }
  
  
  # TABLE PARAMETERS --------------------------------------------------------
  
  rep_row <- nrow(report_df)
  rep_col <- ncol(report_df)
  
  
  # WRITE DATA --------------------------------------------------------------
  
  # -- SUMMARY & TITLE -- #
  openxlsx::writeData(
    wb = wb,
    sheet = sheet_name,
    x = if(!is.null(name)) {
      paste0(name,
             " - ",
             anova_object[["meta_data"]][["anova_type"]],
             " ANOVA Report")
    } else {
      paste0(anova_object[["meta_data"]][["anova_type"]],
             " ANOVA Report")
    },
    startCol = start_col,
    startRow = start_row
  )
  
  
  # Title
  openxlsx::writeData(
    wb = wb,
    sheet = sheet_name,
    x = "Report Table",
    startCol = start_col,
    startRow = start_row + 2
  )
  openxlsx::writeData(
    wb = wb,
    sheet = sheet_name,
    x = report_df,
    startCol = start_col,
    startRow = start_row + 3
  )
  
  
  # Write Note
  openxlsx::writeData(
    wb = wb,
    sheet = sheet_name,
    x = if (!is.na(correction_type)) {
      paste0(
        "NOTE: Running ",
        correction_type,
        " correction. SE = Standard Error; SS = Sum of Squares; df = Degrees of freedom; MS = Mean of Squares."
      ) 
    } else {
      "NOTE: SE = Standard Error; SS = Sum of Squares; df = Degrees of freedom; MS = Mean of Squares."
    },
    startCol = start_col,
    startRow = start_row + 3 + rep_row + 1
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
  apply_anova_report_format(
    wb = wb,
    sheet = sheet_name,
    mod_row = rep_row,
    mod_col = rep_col,
    start_col = start_col,
    start_row = start_row + 2,
    es_length = es_length,
    eff_length = eff_length,
    digits = digits
  )
  
  # Expand column width of col B
  openxlsx::setColWidths(wb,
                         sheet = sheet_name,
                         cols = "B",
                         widths = 15)
  
  # Hide gridlines
  openxlsx::showGridLines(wb,
                          sheet = sheet_name,
                          showGridLines = FALSE)
  
}

