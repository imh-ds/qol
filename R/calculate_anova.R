#' Calculate Between-Subjects ANOVA
#'
#' @param data 
#' @param outcome 
#' @param between_vars 
#' @param covariates 
#' @param interactions 
#' @param posthoc_correction 
#' @param es_type 
#' @param g_correction 
#' @param conf_level 
#' @param emm_weights 
#' 
#' @export
calc_anova <- function(
    
  data,
  outcome,
  between_vars,
  covariates = NULL,
  interactions = FALSE,
  posthoc_correction = "tukey",
  es_type = "hedges",
  conf_level = 0.95,
  emm_weights = TRUE
  
) {
  

  # ERROR MESSAGES ----------------------------------------------------------
  
  if (length(posthoc_correction) > 1) {
    
    stop(
      "More than 1 post-hoc correction method detected.
      Please specify only one post-hoc correction method."
    )
    
  }
  
  # If more than 1 bs var is specified and interactions is FALSE
  if (length(between_vars) > 1 && isFALSE(interactions)) {
    
    message(
      "Interactions cannot be FALSE when more than 1 between-factor is specified. 
      Changing interactions argument to TRUE. 
      If no interaction term is desired, specify additional between-factors as covariates."
    )
    
    interactions <- TRUE
    
  }
  
  
  # If only 1 bs var is specified and interactions is TRUE
  if (length(between_vars) == 1 && isTRUE(interactions)) {
    
    message(
      "Interactions cannot be TRUE when only 1 between-factor is specified.
      Changing interactions argument to FALSE.
      If an interaction term with covariates is desired, specify this manually."
    )
    
    interactions <- FALSE
    
  }
  
  
  # If "*" is detected in manual specification of interactions
  if (!isTRUE(interactions) && !isFALSE(interactions)) {
    
    if (grepl("\\*", interactions)) {
      
      message(
        "Interaction terms need to be specified with ':' instead of '*'.
        Changing '*' to ':'."
      )
      
      interactions <- gsub("\\*", ":", interactions)
      
    }
    
  }
  

  # ANOVA PARAMETERS --------------------------------------------------------

  # Change all between-subjects variables to factor
  data[between_vars] <- lapply(data[between_vars],
                               factor)
  
  # Specify interaction terms if any
  if (isFALSE(interactions)) {
    
    # Define BS Term formula
    bsTerms <- as.formula(
      paste0(" ~ ",
             paste0(c(between_vars),
                    collapse = " + ")))
    
    # ANOVA formula
    formula <- as.formula(
      paste0(outcome,
             " ~ ",
             paste0(c(between_vars,
                      covariates),
                    collapse = " + ")))
    
  } else {
    
    # If interaction is just specified as TRUE instead of defining specific
    # interaction, then automate every interaction across between_vars
    if (isTRUE(interactions)) {
      
      interactions_list <- lapply(
        2:length(between_vars),
        function(n) {
          
          combinations <- combn(between_vars, n)
          
          result <- apply(combinations,
                          2,
                          function(x) paste0(x, collapse = ":"))
          
        }
        
      )
      
      interactions <- purrr::reduce(interactions_list,
                                    c)
      
    }
    
    if (!isTRUE(interactions) && !isFALSE(interactions)) {
      
      # Function to check if any covariate is in an interaction
      check_covariates <- function(interaction, covariates) {
        any(sapply(covariates, grepl, interaction))
      }
      
      # Filter interactions
      bsTerm_int <- interactions[!sapply(interactions, check_covariates, covariates)]
      
      # Define ANOVA formula
      bsTerms <- as.formula(
        paste0(" ~ ",
               paste0(c(between_vars,
                        bsTerm_int),
                      collapse = " + ")))
      
    } else {
      
      # Define BS term formula
      bsTerms <- as.formula(
        paste0(" ~ ",
               paste0(c(between_vars,
                        interactions),
                      collapse = " + ")))
      
    }
    
    # ANOVA formula
    formula <- as.formula(
      paste0(outcome,
             " ~ ",
             paste0(c(between_vars,
                      covariates,
                      interactions),
                    collapse = " + ")))
    
  }
  
  
  
  
  
  
  if (isFALSE(interactions)) {
    
    # Define BS Term formula
    bsTerms <- as.formula(
      paste0(" ~ ",
             paste0(c(between_vars),
                    collapse = " + ")))
    
    # ANOVA formula
    formula <- as.formula(
      paste0(outcome,
             " ~ ",
             paste0(c(between_vars,
                      covariates),
                    collapse = " + ")))
    
  } else {
    
    # If interaction is just specified as TRUE instead of defining specific
    # interaction, then automate every interaction across between_vars
    if (isTRUE(interactions)) {
      
      interactions_list <- lapply(
        2:length(between_vars),
        function(n) {
          
          combinations <- combn(between_vars, n)
          
          result <- apply(combinations,
                          2,
                          function(x) paste0(x, collapse = ":"))
          
        }
        
      )
      
      interactions <- purrr::reduce(interactions_list,
                                    c)
      
    }
    
    if (!isTRUE(interactions) && !isFALSE(interactions)) {
      
      # Function to check if any covariate is in an interaction
      check_covariates <- function(interaction, covariates) {
        any(sapply(covariates, grepl, interaction))
      }
      
      # Filter interactions
      bsTerm_int <- interactions[!sapply(interactions, check_covariates, covariates)]
      
      # Define ANOVA formula
      bsTerms <- as.formula(
        paste0(" ~ ",
               paste0(c(between_vars,
                        bsTerm_int),
                      collapse = " + ")))
      
    } else {
      
      # Define BS term formula
      bsTerms <- as.formula(
        paste0(" ~ ",
               paste0(c(between_vars,
                        interactions),
                      collapse = " + ")))
      
    }
    
    # ANOVA formula
    formula <- as.formula(
      paste0(outcome,
             " ~ ",
             paste0(c(between_vars,
                      covariates,
                      interactions),
                    collapse = " + ")))
    
  }
  
  # List of post-hoc effects
  ph_list <- c(between_vars)
  
  # Create an empty list to store the combinations
  postHoc <- list()
  
  # Generate all combinations for each size from 1 to the length of the vector
  for(i in 1:length(ph_list)) {
    
    # The combn function generates all combinations of size i
    # The simplify = FALSE argument ensures the result is a list
    # The lapply function applies a function to each element of the list
    # In this case, the function is c, which combines the elements into a vector
    combinations_i <- lapply(combn(ph_list,
                                   i,
                                   simplify = FALSE),
                             c)
    
    # Append the combinations to the list
    postHoc <- c(postHoc, combinations_i)
    
  }
  
  
  # Between subjects argument
  bs <- rlang::syms(between_vars)
  
  # Covariates argument
  cov <- rlang::syms(covariates)
  
  # Estimated Marginal Means table argument
  emMeans <- as.formula(
    
    # Paste in ~
    paste0(
      "~",
      lapply(
        seq_along(postHoc),
        function(i) {
          
          # Collapse interactions with ":"
          eff <- paste0(postHoc[[i]],
                        collapse = ":")
          
          # Return
          return(eff)
          
        }
      ) %>% 
        
        # Unlist the list
        unlist() %>% 
        
        # Collapse strings with "+"
        paste0(.,
               collapse = " + ")
      
    )
  )
  
  
  # ANOVA -------------------------------------------------------------------
  
  # Run One-Way ANOVA
  aov <- jmv::ancova(
    data = data,
    formula = formula,
    covs = NULL,
    effectSize = c("eta", "partEta", "omega"),
    modelTest = TRUE,
    homo = TRUE,
    norm = TRUE,
    postHoc = bsTerms,
    postHocCorr = posthoc_correction,
    emMeans = bsTerms,
    emmPlots = FALSE,
    emmTables = TRUE,
    emmWeights = emm_weights,
    ciWidthEmm = conf_level * 100
  )
  
  
  # ANOVA SUMMARY -----------------------------------------------------------
  
  # Between-Subjects Summary Table
  bs_sum <- aov[["main"]][["asDF"]] %>% 
    
    # Calculate Cohen's f
    dplyr::mutate(
      cohen_f = sqrt(etaSqP / (1 - etaSqP))
    )
  
  # Remove rownames
  rownames(bs_sum) <- NULL
  
  
  
  # ASSUMPTIONS -------------------------------------------------------------
  
  # -- TEST OF NORMALITY (SHAPIRO-WILK) -- #
  bs_norm <- aov[["assump"]][["norm"]][["asDF"]] %>% 
    
    # Remove column affixes
    magrittr::set_colnames(
      ., gsub("\\[sw\\]", "", colnames(.)
      )
    ) %>% 
    
    # Mutate new outcome column
    dplyr::mutate(
      outcome = outcome
    ) %>% 
    
    # Reorder
    dplyr::relocate(
      outcome,
      .before = s
    )
    
  rownames(bs_norm) <- NULL
  
  # -- HOMOGENEITY OF VARIANCES TEST (LEVENE'S) -- #
  bs_var <- aov[["assump"]][["homo"]][["asDF"]] %>% 
    
    # Mutate new outcome column
    dplyr::mutate(
      outcome = outcome
    ) %>% 
    
    # Reorder
    dplyr::relocate(
      outcome,
      .before = `F`
    )
  
  rownames(bs_var) <- NULL
  
  
  # ESTIMATED MARGINAL MEANS ------------------------------------------------
  
  # Get EMM
  emm <- lapply(
    
    seq_along(postHoc),
    function(i) {
      
      # Get effect
      eff <- postHoc[[i]]
      
      # Get effect name
      eff_name <- paste0(eff,
                         collapse = "*")
      
      # Get EMM table
      emm_tab <- aov[["emm"]][[i]][[2]][["asDF"]]
      
      # For every row...
      emm_n <- lapply(seq(nrow(emm_tab)),
                      function(n) {
                        
                        # Copy data
                        dat <- data
                        
                        # Get values of the group
                        grp <- unlist(emm_tab[n,][1:(length(eff))])
                        
                        # Loop for each group value and update dat
                        for (g in seq_along(grp)) {
                          
                          val <- grp[[g]]
                          nm <- names(grp[g])
                          
                          dat <- dat %>% 
                            filter(!!sym(nm) == val)
                          
                        }
                        
                        # Get sample size
                        n <- nrow(dat)
                        
                        # Return
                        return(n)
                        
                      }
                      
      ) %>% 
        
        # Reduce
        unlist()
      
      # Add to EMM Table & rename
      emm_tab <- emm_tab %>% 
        
        # Create sample size n and effect
        dplyr::mutate(n = emm_n,
                      effect = eff_name)
      
      # Create NA row
      empty_row <- data.frame(matrix(ncol = ncol(emm_tab),
                                     nrow = 1))
      colnames(empty_row) <- colnames(emm_tab)
      
      # Add row to emm_tab
      emm_tab <- emm_tab %>% 
        dplyr::add_row(empty_row)
      
      # Return
      return(emm_tab)
      
    }
    
  )
  
  # Reduce
  emm_table <- purrr::reduce(emm,
                             dplyr::bind_rows) %>% 
    
    dplyr::select(effect,
                  setdiff(names(.),
                          c("effect", "n", "mean", "se", "lower", "upper")),
                  n, mean, se, lower, upper) %>% 
    
    # Remove last row
    dplyr::filter(dplyr::row_number() <= n()-1)
  
  # Remove rownames
  rownames(emm_table) <- NULL
  
  
  
  # POST-HOC TESTS ----------------------------------------------------------
  
  # Get EMM Contrasts
  emmc <- lapply(
    
    seq_along(postHoc),
    function(i) {
      
      # Get length of post-hoc comparison
      ph_len <- length(postHoc[[i]])
      
      # Get post-hoc effect
      ph_eff <- postHoc[[i]]
      
      # Get effect name
      ph_name <- if(length(ph_eff) > 1) {
        
        paste0(ph_eff, collapse = "*")
        
      } else {
        
        ph_eff
        
      }
      
      # Create descriptive dataframe for post-hoc contrast effects
      ph_desc <- emm[[i]] %>% 
        
        # Ensure selection order is the same as EMM
        dplyr::select(dplyr::all_of(postHoc[[i]]),
                      dplyr::everything()) %>% 
        
        # Create new groups
        dplyr::mutate(effect = ph_name,
                      group_1 = apply(.[1:ph_len], 1, 
                                      paste, 
                                      collapse = " "),
                      group_2 = group_1) %>% 
        
        # Remove last row
        dplyr::filter(dplyr::row_number() <= n()-1)
      
      # Recreate for just group 1
      ph_desc_g1 <- ph_desc %>% 
        
        # Select necessary vars
        dplyr::select(group_1, mean, se, n) %>% 
        
        dplyr::rename(
          g1_mean = mean,
          g1_se = se,
          g1_n = n
        )
      
      # Recreate for just group 2
      ph_desc_g2 <- ph_desc %>% 
        
        # Select necessary vars
        dplyr::select(group_2, mean, se, n) %>% 
        
        dplyr::rename(
          g2_mean = mean,
          g2_se = se,
          g2_n = n
        )
      
      
      # Get post-hoc table
      ph_table <- aov[["postHoc"]][[i]][["asDF"]] %>% 
        
        # Create new groups
        dplyr::mutate(effect = ph_name,
                      group_1 = apply(.[1:ph_len], 1, 
                                      paste, 
                                      collapse = " "),
                      group_2 = apply(.[(ph_len+2):(ph_len*2+1)], 
                                      1, 
                                      paste, 
                                      collapse = " ")) %>% 
        
        # Unselect default post-hoc variable columns from jamovi
        dplyr::select(-(1:(ph_len*2+1))) %>% 
        
        # Reorder
        dplyr::select(effect,
                      group_1,
                      group_2,
                      dplyr::everything()) %>% 
        
        # Join with descriptives
        dplyr::inner_join(ph_desc_g1, by = "group_1") %>% 
        dplyr::inner_join(ph_desc_g2, by = "group_2")
      
      
      # Get vector of group combinations for every respondent
      int_combos <- do.call(base::interaction,
                            c(data[ph_eff],
                              sep = " "))
      
      # Create working dataframe
      wrk_df <- data.frame(
        
        group = int_combos,
        outcome = data[[outcome]]
        
      )
      
      # Get effect descriptives & effect sizes
      eff_desc <- lapply(
        seq(nrow(ph_table)),
        function(i) {
          
          # Get ESM parameters
          m1 <- ph_table[[i, "g1_mean"]]
          m2 <- ph_table[[i, "g2_mean"]]
          
          se1 <- ph_table[[i, "g1_se"]]
          se2 <- ph_table[[i, "g2_se"]]
          
          n1 <- ph_table[[i, "g1_n"]]
          n2 <- ph_table[[i, "g2_n"]]
          
          sd1 <- se1 * sqrt(n1)
          sd2 <- se2 * sqrt(n2)
          
          
          # Get names of groups 1 & 2
          group1 <- ph_table[[i, "group_1"]]
          group2 <- ph_table[[i, "group_2"]]
          
          # Identify pair
          pair <- c(group1,
                    group2)
          
          # Apply filter to pairwise comparison groups
          pc_df <- wrk_df %>% 
            dplyr::filter(sapply(pair,
                                 grepl,
                                 group) %>%
                            apply(1, any))
          
          g1 <- dplyr::filter(pc_df,
                              group == group1)
          
          g2 <- dplyr::filter(pc_df,
                              group == group2)
          
          
          # MSE for between subjects
          mse <- subset(bs_sum,
                        name == "Residuals")[["ms"]]
          
          # Calculate RMSE
          rmse <- sqrt(mse)
          
          
          # Calculate effect size
          es <- calc_esm(
            
            mean = c(m1, m2),
            sd = c(sd1, sd2),
            n = c(n1, n2),
            sd_pooled = rmse,
            paired = FALSE,
            conf_level = conf_level,
            type = es_type
            
          )
          
          # Compile
          desc_df <- data.frame(
            
            "group_1" = group1,
            "group_2" = group2,
            es_type = es[["es_type"]],
            es = es[["es"]],
            es_se = es[["es_se"]],
            es_ci_lower = es[["es_ci_lower"]],
            es_ci_upper = es[["es_ci_upper"]]
            
          )
          
          # Return
          return(desc_df)
          
        }
      )
      
      # Reduce
      eff_table <- purrr::reduce(eff_desc,
                                 rbind)
      
      # Combine
      emmc_table <- suppressMessages(
        
        dplyr::inner_join(ph_table,
                          eff_table)
        
      )
      
      # Create NA row
      empty_row <- data.frame(matrix(ncol = ncol(emmc_table),
                                     nrow = 1))
      colnames(empty_row) <- colnames(emmc_table)
      
      # Add row to emmc_table
      emmc_table <- emmc_table %>%
        dplyr::add_row(empty_row)
      
      # Return
      return(emmc_table)
      
    }
    
  )
  
  # Reduce into EMM-C Table
  emmc_table <- purrr::reduce(emmc,
                              dplyr::bind_rows) %>% 
    
    # Remove last row
    dplyr::filter(dplyr::row_number() <= n()-1)
  
  
  
  # META-DATA ---------------------------------------------------------------
  
  # Compile meta-data
  meta_data <- list(
    
    anova_type = dplyr::case_when(
      length(between_vars) == 1 ~ "One-way",
      length(between_vars) > 1 ~ "Factorial"
    ),
    outcome = outcome,
    between_vars = between_vars,
    covariates = covariates,
    interactions = interactions,
    posthoc_correction = posthoc_correction,
    es_type = es_type,
    conf_level = conf_level
    
  )
  
  # COMPILE INTO RESULTS ----------------------------------------------------
  
  # Compile results
  return_list <- list(
    
    bs_summary = bs_sum,
    bs_norm = bs_norm,
    bs_var = bs_var,
    emm = emm_table,
    emmc = emmc_table,
    meta_data = meta_data
    
  )
  
  # Return
  return(return_list)
  
}