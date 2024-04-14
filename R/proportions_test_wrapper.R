#' Proportions Test Wrapper
#' 
#' @description
#' A simple wrapper that automates the running and extracting of common report
#' metrics for a chi-square independent proportions test.
#' Automatically applies holm p adjustment to multiple comparisons. Returns
#' model statistic and also pairwise comparison results. 
#' 
#' @param data A dataframe object. This should be a structured dataset where
#' each column represents a variable and each row represents an observation.
#' @param group A single character string of the group variable name. The
#' variable should not be factored with different levels. The current wrapper
#' does not support this capability. 
#' @param outcomes Vector of outcome variable names. The outcomes should have
#' the same levels (e.g., all 0's and 1's, or all 1's and 2's). 
#' 
#' @returns A list of dataframes containing the chi-square proportions test
#' results, the proportions of each group's levels, and the pairwise comparison
#' results. 
#' 
#' @examples
#' data <- carData::WVS %>% 
#'    mutate(across(.cols = c(poverty, religion, degree, gender),
#'                  .fns = function(x) as.integer(x)),
#'           country = as.character(country))
#' 
#' proportion_test(data = data,
#'                 group = "country",
#'                 outcomes = c("degree",
#'                              "gender"))
#' 
#' @export
proportion_test <- function(data = .,
                            group,
                            outcomes){
  
  # Make empty lists
  stat_list <- list()
  prop_list <- list()
  pairwise_list <- list()
  
  # Get unique number of groups
  group_pairs <- combn(unique(data[[group]]), 2, simplify = FALSE)
  
  
  for(ovar in outcomes){
    
    # Create crosstab
    table <- table(data[[group]],
                   data[[ovar]])
    
    # Run proportions test
    result <- prop.test(table,
                        correct = T)
    
    # Get proportions test statistics dataframe
    stats <- data.frame(chisq = result$statistic,
                        df = result$parameter,
                        p = result$p.value,
                        method = result$method) %>% 
      mutate(text = paste0("X2(",
                           df,
                           ") = ",
                           sprintf('%.3f', chisq),
                           ", p ",
                           ifelse(p < .001,
                                  "< .001", paste0("= ", sprintf('%.3f', p))),
                           ")")) %>% 
      rownames_to_column(var = "Outcome") %>% 
      mutate(Outcome = ovar)
    
    # Get percentage proportion
    proportions <- result$estimate %>% 
      as.data.frame() %>% 
      rename(Proportion_grp1 = ".") %>% 
      rownames_to_column(var = "Outcome") %>% 
      mutate(Outcome = ovar,
             Proportion_grp2 = 1-Proportion_grp1)
    
    # Join together
    table <- table %>% 
      as.data.frame() %>% 
      pivot_wider(names_from = Var2, values_from = Freq) %>% 
      rename(Group = "Var1") %>% 
      cbind(proportions)
    
    
    # Load into list
    stat_list[[ovar]] <- stats
    prop_list[[ovar]] <- table
    
    
    
    # ------------------------- #
    # -- PAIRWISE COMPARISON -- #
    # ------------------------- #
    
    for(pair in seq_along(group_pairs)){
      
      # Define groups
      group1 <- group_pairs[[pair]][[1]]
      group2 <- group_pairs[[pair]][[2]]
      
      # Subset the data for the current pair of groups
      data_subset <- data %>% filter(!!sym(group) %in% c(group1, group2))
      
      # Create crosstab
      table_pair <- table(data_subset[[group]],
                          data_subset[[ovar]])
      
      # Run proportions test
      result_pair <- prop.test(table_pair,
                               correct = T)
      
      # Get proportions test statistics dataframe
      stats_pair <- data.frame(chisq = result_pair$statistic,
                               df = result_pair$parameter,
                               p = result_pair$p.value) %>% 
        rownames_to_column(var = "Outcome") %>% 
        mutate(Outcome = ovar,
               Group_1 = group_pairs[[pair]][[1]],
               Group_2 = group_pairs[[pair]][[2]]) %>% 
        select(Outcome, Group_1, Group_2, everything())
      
      # Save to list
      name <- paste0(ovar,
                     "_",
                     paste0(pair, collapse = "_"))
      pairwise_list[[name]] <- stats_pair
      
    }
    
  }
  
  # Reduce to working export dataframe
  stat_export <- purrr::reduce(stat_list, rbind)
  prop_export <- purrr::reduce(prop_list, rbind)
  pairwise_export <- purrr::reduce(pairwise_list, rbind)
  
  
  # Correct p value in pairwise
  corrected_p <- list()
  
  # Loop separately for every outcome
  for(ovar in outcomes){
    
    # Filter to specific outcome
    holm_df <- pairwise_export %>%
      filter(Outcome %in% ovar)
    
    # Apply Holm p adjustment
    holm_p <- p.adjust(holm_df$p,
                       method = "holm") %>% 
      as.data.frame() %>% 
      rename("p_holm" = ".")
    
    # Assign
    corrected_p[[ovar]] <- holm_p
    
  }
  
  # Reduce to single dataframe
  corrected_p_df <- purrr::reduce(corrected_p,
                                  rbind)
  
  # Merge with pairwise export dataframe and apply pasteable text
  pairwise_export <- pairwise_export %>% 
    cbind(corrected_p_df) %>% 
    mutate(text = paste0("X2(",
                         df,
                         ") = ",
                         sprintf('%.3f', chisq),
                         ", p_holm ",
                         ifelse(p < .001,
                                "< .001", paste0("= ", sprintf('%.3f', p_holm))),
                         ")"))
  
  # Aggregate to reportable sheet
  results_sheet <- list(stat = stat_export,
                        prop = prop_export,
                        pair = pairwise_export)
  
  # Return results
  return(results_sheet)
  
}
