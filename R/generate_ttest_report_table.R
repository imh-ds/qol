#' Generate T-Test Reportable Table Sheet
#'
#' @param ttest_object The t-test object from \code{wrap_ttest}.
#' @param report_variance A string value or vector of strings indicating which
#'   variance measure to report. Options include \code{"sd"} for standard
#'   deviation, \code{"se"} for standard error, or \code{c("sd", "se")} for
#'   both. The default is standard deviation \code{"sd"}.
#' @param report_test A string value indicating which t-test statistic to
#'   report. Options include \code{"student"} for Student's t-test,
#'   \code{"welch"} for Welch's t-test, and \code{"wilcox"} for Wilcoxon
#'   Rank-Sum W test / Mann-Whitney U test. The default is \code{"welch"}.
#'
generate_ttest_report <- function(
    ttest_object,
    report_variance = "sd",
    report_test = "welch"
) {
  
  # Get names of effect size
  desc <- ttest_object[["descriptive"]]
  
  
  # t-test type
  ttest_type <- dplyr::case_when(
    report_test == "student" ~ "Student's t",
    report_test == "welch" ~ "Welch's t",
    report_test == "wilcox" ~ "Wilcoxon Rank-Sum W"
  )
  
  ttest_stat <- dplyr::case_when(
    report_test %in% c("student", "welch") ~ "t",
    report_test == "wilcox" ~ "W"
  )
  
  
  # Get names of effect size
  es_name <- dplyr::case_when(
    ttest_object[["meta_data"]][["es_type"]] == "hedges" ~ "g",
    ttest_object[["meta_data"]][["es_type"]] == "cohen" ~ "d",
    ttest_object[["meta_data"]][["es_type"]] == "glass" ~ "\u0394"
  )
  
  es_stat <- dplyr::case_when(
    ttest_object[["meta_data"]][["es_type"]] == "hedges" ~ "Hedge's g",
    ttest_object[["meta_data"]][["es_type"]] == "cohen" ~ "Cohen's d",
    ttest_object[["meta_data"]][["es_type"]] == "glass" ~ "Glass's \u0394"
  )
  
  
  # Create t-test statistics
  ttest_stats <- ttest_object[["ttest"]] %>% 
    
    # Filter to selected t-test type
    dplyr::filter(test == !!ttest_type) %>% 
    
    # Select relevant variables
    dplyr::select(
      outcome, 
      df, 
      stat, 
      p, 
      es
    )
  
  
  # If standard error is NOT selected:
  if (!"se" %in% report_variance) {
    
    ttest_desc <- ttest_object[["descriptive"]] %>% 
      dplyr::select(-se) %>% 
      dplyr::rename(SD = sd)
    
  }
  
  # If standard deviation is NOT selected:
  if (!"sd" %in% report_variance) {
    
    ttest_desc <- ttest_object[["descriptive"]] %>% 
      dplyr::select(-sd) %>% 
      dplyr::rename(SE = se)
    
  }
  
  # If both standard error and deviation are selected:
  if (all(c("sd", "se") %in% report_variance)) {
    
    ttest_desc <- ttest_object[["descriptive"]] %>% 
      dplyr::rename(SD = sd,
                    SE = se)
    
  }
  
  
  # Create report data frame
  report_df <- ttest_desc %>% 
    
    # Remove duplicated outcomes
    dplyr::mutate(
      outcome = ifelse(!duplicated(outcome), 
                       outcome, 
                       NA)
    ) %>% 
    
    # Join with t-test statistics
    dplyr::full_join(
      ttest_stats, 
      by = "outcome"
    ) %>% 
    
    # Rename
    dplyr::rename(
      Outcome = outcome,
      Group = group,
      Mean = mean,
      !!rlang::sym(ttest_stat) := stat,
      !!rlang::sym(es_name) := es
    )
  
  # Return
  return(report_df)
  
}