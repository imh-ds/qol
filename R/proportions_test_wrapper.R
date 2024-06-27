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
proportion_test <- function(
    
    data = .,
    group,
    outcomes,
    digits = 3
    
){
  
  # Rounding
  rnd <- paste0("%.",
                digits,
                "f")
  
  # Get unique number of groups
  group_pairs <- combn(
    unique(data[[group]]),
    2,
    simplify = FALSE
  )
  
  prop_list <- lapply(
    
    outcomes,
    function(ovar) {
      
      # Create crosstab
      table <- base::table(data[[group]],
                           data[[ovar]])
      
      # Run proportions test
      result <- stats::prop.test(table,
                                 correct = T)
      
      # Get proportions test statistics dataframe
      stats <- base::data.frame(
        
        chisq = result[["statistic"]],
        df = result[["parameter"]],
        p = result[["p.value"]],
        method = result[["method"]]
        
      ) %>% 
        
        # Create reportable text
        dplyr::mutate(
          
          text = base::paste0(
            
            "(\u03C7\u00B2(",
            df,
            ") = ",
            sprintf(rnd, chisq),
            ", p ",
            ifelse(p < .001,
                   "<.001", 
                   paste0("= ", 
                          sprintf(rnd, p))),
            ")"
            
          ),
          
          outcome = ovar
        ) %>% 
        
        # Relocate
        dplyr::relocate(
          outcome,
          .before = chisq
        )
      
      
      # Get percentage proportion
      proportions <- base::as.data.frame(result[["estimate"]]) %>% 
        
        # Rename
        magrittr::set_colnames(.,
                               "prop_grp1") %>% 
        
        # Create necessary vars
        dplyr::mutate(outcome = c(ovar,
                                  rep(NA, nrow(.) - 1)),
                      prop_grp2 = 1 - prop_grp1)
      
      # Join together
      table <- as.data.frame(table) %>% 
        
        # Pivot
        tidyr::pivot_wider(names_from = Var2,
                           values_from = Freq) %>% 
        
        # Rename
        dplyr::rename(group = "Var1") %>% 
        
        # Combine
        base::cbind(proportions) %>% 
        
        # Relocate
        dplyr::relocate(
          outcome,
          .before = group
        )
      
      
      
      # ------------------------- #
      # -- PAIRWISE COMPARISON -- #
      # ------------------------- #
      
      pair_list <- lapply(
        seq_along(group_pairs),
        function(pair) {
          
          # Define groups
          group1 <- group_pairs[[pair]][[1]]
          group2 <- group_pairs[[pair]][[2]]
          
          # Subset the data for the current pair of groups
          data_subset <- data %>% 
            dplyr::filter(!!sym(group) %in% c(group1, group2))
          
          # Create crosstab
          table_pair <- table(data_subset[[group]],
                              data_subset[[ovar]])
          
          # Run proportions test
          result_pair <- prop.test(table_pair,
                                   correct = T)
          
          # Get proportions test statistics dataframe
          stats_pair <- data.frame(
            
            chisq = result_pair[["statistic"]],
            df = result_pair[["parameter"]],
            p = result_pair[["p.value"]]
            
          ) %>% 
            
            # Create new variables
            dplyr::mutate(outcome = ovar,
                          group_1 = group_pairs[[pair]][[1]],
                          group_2 = group_pairs[[pair]][[2]]) %>% 
            
            # Reorder
            dplyr::select(outcome, 
                          group_1, 
                          group_2, 
                          dplyr::everything())
          
          # Save to list
          return(stats_pair)
          
        }
      )
      
      # Reduce
      pairs <- purrr::reduce(
        
        pair_list,
        rbind
        
      )
      
      # Load into list
      return_list <- list(
        
        stats_list = stats,
        prop_list = table,
        pair_list = pairs
        
      )
      
    }
    
  )
  
  # Extract the results
  stats <- lapply(prop_list, 
                  function(x) x[["stats_list"]])
  props <- lapply(prop_list, 
                  function(x) x[["prop_list"]])
  pairs <- lapply(prop_list, 
                  function(x) x[["pair_list"]])
  
  # Reduce the results
  stats_table <- purrr::reduce(stats,
                               rbind)
  props_table <- purrr::reduce(props,
                               rbind)
  pairs_table <- purrr::reduce(pairs,
                               rbind)
  
  # Remove rownames
  rownames(stats_table) <- NULL
  rownames(props_table) <- NULL
  rownames(pairs_table) <- NULL
  
  
  # Correct p value in pairwise
  corrected_p <- list()
  
  corrected_p <- lapply(
    outcomes,
    function(ovar) {
      
      # Filter to specific outcome
      holm_df <- pairs_table %>%
        dplyr::filter(outcome %in% ovar)
      
      # Apply Holm p adjustment
      holm_p <- stats::p.adjust(holm_df$p,
                                method = "holm") %>% 
        base::as.data.frame() %>% 
        dplyr::rename("p_holm" = ".")
      
      # Return
      return(holm_p)
      
    }
  )
  
  # Reduce to single dataframe
  corrected_p_df <- purrr::reduce(corrected_p,
                                  rbind)
  
  # Merge with pairwise export dataframe and create reportable text
  pairs_table <- pairs_table %>% 
    
    # Bind columns
    cbind(corrected_p_df) %>% 
    
    # Create reportable text
    dplyr::mutate(
      text = paste0(
        "(\u03C7\u00B2(",
        df,
        ") = ",
        sprintf(rnd, 
                chisq),
        ", p_holm ",
        ifelse(p < .001,
               "<.001", 
               paste0("= ", 
                      sprintf(rnd,
                              p_holm)
               )
        ),
        ")"
      )
    ) %>% 
    
    # Group by outcome
    dplyr::group_by(outcome) %>% 
    
    # Keep only first case of outcome
    dplyr::mutate(outcome = base::replace(outcome,
                                          dplyr::row_number() > 1,
                                          NA)) %>% 
    
    # Ungroup
    dplyr::ungroup() %>% 
    
    # Revert back to dataframe from tibble
    base::as.data.frame()
  
  # Aggregate to reportable sheet
  results_sheet <- list(
    stat = stats_table,
    prop = props_table,
    pair = pairs_table
  )
  
  # Return results
  return(results_sheet)
  
}
