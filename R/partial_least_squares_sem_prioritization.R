#' Partial Least Squares Structural Equation Modeling Prioritization
#'
#' @param loadings 
#' @param weights 
#' @param data 
#' @param total_estimates 
#' @param outcomes 
#' @param weight_type 
#' @param cor_method 
#'
#' @return
#' @export
#'
#' @examples
plssem_prioritization <- function(
    loadings = NULL,
    weights = NULL,
    data,
    total_estimates,
    outcomes,
    weight_type = "combo",
    cor_method = "pearson") {
  
  # Check if both arguments are empty
  if (length(loadings) == 0 && length(weights) == 0) {
    
    # Stop the function and display an error message
    stop("Both arguments 'loadings' and 'weights' are empty. Please provide at least one.")
    
  }
  
  
  # If loadings are specified
  if (!is.null(loadings)) {
    
    ld <- loadings %>% 
      
      # Filter out interaction effects
      dplyr::filter(!stringr::str_detect(path, 
                                         stringr::fixed("\u00D7"))) %>% 
      
      # Split out indicator and outcome from `path` var
      dplyr::mutate(indicator = stringr::str_split_i(path,
                                                     "  ->  ",
                                                     1),
                    composite = stringr::str_split_i(path,
                                                     "  ->  ",
                                                     2))
    
    # Set working data
    wd <- ld
    
  }
  
  
  # If weights are specified
  if(!is.null(weights)) {
    
    wt <- weights %>% 
      
      # Filter out interaction effects
      dplyr::filter(!stringr::str_detect(path, 
                                         stringr::fixed("\u00D7"))) %>% 
      
      # Split out indicator and outcome from `path` var
      dplyr::mutate(indicator = stringr::str_split_i(path,
                                                     "  ->  ",
                                                     1),
                    composite = stringr::str_split_i(path,
                                                     "  ->  ",
                                                     2))
    
    wt_list <- base::lapply(
      base::unique(wt[["composite"]]),
      function(c) {
        
        norm_wgt <- wt %>% 
          dplyr::filter(composite == c) %>% 
          dplyr::mutate(boot_est = boot_est / base::mean(boot_est),
                        boot_se = boot_se / base::mean(boot_se))
        
      }
    )
    
    # Reduce & Return
    wt <- purrr::reduce(wt_list,
                        rbind)
    
    # Set working data
    wd <- wt
    
  }
  
  
  # If both loadings and weights are specified
  if(!is.null(loadings) && !is.null(weights)) {
    
    wd <- as.data.frame(
      
      # Bind loadings and weights working data
      base::rbind(ld, wt) %>% 
        
        # Group by path
        dplyr::group_by(path) %>% 
        
        # Combine loadings & weights together
        dplyr::summarise(
          
          # For numeric variables
          dplyr::across(dplyr::where(is.numeric), 
                        mean, 
                        na.rm = TRUE),  
          
          # For character variables
          dplyr::across(dplyr::where(is.character), 
                        ~dplyr::first(stats::na.omit(.x)))  
        ) %>% 
        
        # Ungroup
        dplyr::ungroup())
    
  }
  
  
  # Identify lower-order composite (LOC) variables
  wd_loc <- base::intersect(wd[["indicator"]],
                            wd[["composite"]])
  
  # If there are LOC, then weight manifest indicator loadings
  if (base::length(wd_loc) > 0) {
    
    # Get indicator names
    wd_ind_names <- base::unique(wd[["indicator"]])
    
    
    # Identify and retain just manifest indicator names
    ind_names <- base::setdiff(wd_ind_names,
                               wd_loc)
    
    
    # Calculate and extract LOC loading weights
    wd_wgt <- wd %>% 
      
      # filter to just LOC
      dplyr::filter(indicator %in% wd_loc) %>% 
      
      # Calculate LOC weights based on specified weight type
      dplyr::mutate(se_std = base::mean(boot_se) / boot_se,
                    
                    # Calculate conditional weight
                    wd_wgt = dplyr::case_when(
                      
                      weight_type == "se" ~ se_std / base::mean(se_std),
                      weight_type == "est" ~ boot_est / base::mean(boot_est),
                      weight_type == "combo" ~ ((se_std / base::mean(se_std)) + (boot_est / base::mean(boot_est))) / 2
                      
                    )) %>% 
      
      # Select just the needed vars
      dplyr::select(indicator,
                    wd_wgt) %>% 
      
      # Rename for merging
      dplyr::rename(composite = indicator)
    
    
    # Merge and calculate estimate to use
    wd <- dplyr::full_join(wd, wd_wgt,
                           by = "composite") %>% 
      
      # Calculate weighted loading
      dplyr::mutate(wgt_est = boot_est * wd_wgt,
                    loading = base::ifelse(!base::is.na(wd_wgt), 
                                           wd_wgt, 
                                           boot_est))
    
  } else {
    
    # Calculate estimate to use
    wd <- wd %>% 
      
      # Create duplicate of bootstrapped estimate
      dplyr::mutate(loading = boot_est)
    
  }
  
  # Get LOC DF to look up their respective composite
  lookup <- wd %>% 
    
    # Keep unique combinations of indicator & composite
    dplyr::distinct(indicator, composite) %>% 
    
    # Filter to retain LOC rows
    dplyr::filter(indicator %in% composite)
  
  
  # Fix `composites` of individual indicators to reflect the broader composites
  # instead of LOC
  wd <- wd %>% 
    
    # Join `wd` with `lookup` to match composites
    dplyr::left_join(lookup, 
                     by = c("composite" = "indicator")) %>% 
    
    # If NA, use original composite value, otherwise replace LOC
    dplyr::mutate(composite = base::ifelse(base::is.na(composite.y),
                                           composite,
                                           composite.y)) %>% 
    
    # Unselect lookup value
    dplyr::select(-composite.y)
  
  
  # Create a total effects dataframe to merge
  total_effects <- total_estimates %>% 
    
    # Extract composite and outcomes
    dplyr::mutate(composite = stringr::str_split_i(path,
                                                   "  ->  ",
                                                   1),
                  outcome = stringr::str_split_i(path,
                                                 "  ->  ",
                                                 2),
                  
                  # Mutate a total effect estimate copy
                  total_est = boot_est) %>% 
    
    # Only keep observations with total effects on the outcome
    dplyr::filter(outcome %in% outcomes,
                  
                  # Remove interaction effects
                  !stringr::str_detect(path,
                                       stringr::fixed("\u00D7"))) %>% 
    
    # Keep only relevant composite and total estimate variables
    dplyr::select(composite,
                  outcome,
                  total_est)
  
  
  # Merge total effects into working wd data frame
  wd <- dplyr::inner_join(wd,
                          total_effects,
                          by = "composite",
                          relationship = "many-to-many") %>% 
    
    # Remove rows with LOC
    dplyr::filter(!(indicator %in% wd_loc))
  
  
  
  # -- RUN CORRELATIONS FOR HALO-EFFECT WEIGHTING -- #
  
  # Get indicators to use in creating correlation matrix
  indicators <- base::unique(wd[["indicator"]])
  
  # Create dataframe of just manifest indicator variables
  cor_df <- data %>% 
    dplyr::select(dplyr::all_of(indicators))
  
  
  # Create correlation matrix dataframe
  cor_mat <- cor(cor_df,
                 use = "pairwise.complete.obs",
                 method = cor_method)
  
  # Remove diagonal
  diag(cor_mat) <- NA
  
  
  # Calculate Halo-Effect weighting
  cor_mat <- base::as.data.frame(cor_mat)
  
  # Calculate mean values of correlation matrix
  cor_mean_vals <- cor_mat %>% 
    dplyr::summarise(
      dplyr::across(
        dplyr::everything(),
        \(x) base::mean(x, na.rm = T)
      )
    )
  
  # Transpose & convert to data frame
  cor_tr_df <- as.data.frame(
    
    base::t(cor_mean_vals)
    
  )
  
  # Create halo-effect data frame
  cor_halo <- cor_tr_df %>% 
    tibble::rownames_to_column(
      var = "indicator"
    ) %>% 
    dplyr::rename(
      halo = "V1"
    )
  
  
  
  # -- CALCULATE PLS-SEM PRIORITIZATION SCORES -- #
  
  ind_scores_list <- lapply(
    
    base::unique(wd[["outcome"]]),
    function(o) {
      
      wd_odf <- wd %>% 
        dplyr::filter(outcome == o)
      
      # Calculate indicator total effect scores
      ind_scores <- wd_odf %>% 
        dplyr::mutate(ind_scores = loading * total_est) %>% 
        dplyr::pull(ind_scores)
      
      
      # Calculate indicator-weighted halo scores
      halo_scores <- base::as.data.frame(
        
        base::sweep(cor_mat,
                    2,
                    ind_scores,
                    `*`) %>% 
          dplyr::summarise(
            dplyr::across(
              dplyr::everything(),
              \(x) base::mean(x, na.rm = T)
            )
          )
      )
      
      # Transpose & Convert to working data frame
      halo_scores <- base::as.data.frame(
        base::t(halo_scores)) %>% 
        tibble::rownames_to_column(var = "indicator") %>% 
        dplyr::rename(halo = "V1")
      
      
      # Merge into working wd data
      wd_odf <- dplyr::inner_join(wd_odf,
                                  halo_scores,
                                  by = "indicator")
      
      # Return
      return(wd_odf)
      
    }
    
  )
  
  # Merge back
  wd <- purrr::reduce(ind_scores_list,
                      base::rbind)
  
  # Calculate optimization
  prioritization_list <- lapply(
    
    base::unique(wd[["outcome"]]),
    function(o) {
      
      # Filter to each outcome
      wd_odf <- wd %>% 
        dplyr::filter(outcome == o)
      
      # Calculate prioritization score
      prioritization <- wd_odf %>% 
        
        # Calculate prioritization scores and index
        dplyr::mutate(prioritization = loading * total_est * halo,
                      !!sym(o) := prioritization / base::mean(prioritization)) %>% 
        
        # Select indicator and prioritization index
        dplyr::select(indicator,
                      all_of(o)) %>% 
        
        # Arrange in descending
        dplyr::arrange(dplyr::desc(!!sym(o)))
      
      # Return
      return(prioritization)
      
    }
    
  )
  
  # Merge back
  prioritization <- base::suppressMessages(
    purrr::reduce(prioritization_list,
                  dplyr::inner_join)
  )
  
  # Return prioritization by index
  return(prioritization)
  
}