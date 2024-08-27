#' Repeated Measures Analysis of Variance (ANOVA) Wrapper
#'
#' @description A wrapper for running Repeated Measures Analysis of Variance
#'   (ANOVA).
#'
#' @param data A dataframe object. This should be a structured dataset where
#'   each column represents a variable and each row represents an observation.
#' @param rm A character value indicating the key (i.e., name) of the repeated
#'   measure variables.
#' @param value An optional character to specify the name of the scores of the
#'   repeated measures.
#' @param id A required character indicating the observation ID variable when
#'   running repeated-measures ANOVA.
#' @param within_vars Required characters indicating the within-subjects or
#'   repeated variables when running repeated-measures ANOVA.
#' @param between_vars Optional character(s) indicating the factor grouping
#'   variable(s).
#' @param covariates An optional vector of variable names representing
#'   covariates of interest.
#' @param interactions Either a logical or vector of characters representing the
#'   interactions effects of interest. If specified as \code{TRUE}, all possible
#'   interactionss between the groups are run (not recommended). If specified as
#'   \code{FALSE}, no interactions terms are run. If specified with interactions
#'   terms, e.g., \code{c("treatment\*gender", "treatment\*age_group",
#'   "treatment\*gender\*age_group")}, the only the specified interactions terms
#'   are run. The default is \code{FALSE}.
#' @param correction An optional character indicating what sphericity correction
#'   to use. Choose between "none", "tukey", "scheffe", "bonf", and "holm"
#' @param es_type Character indicating which effect size to return. Options
#'   include \code{"cohen"} for Cohen's d, \code{"hedges"} for Hedges' g, or
#'   \code{"glass"} for Glass's delta. The default is Hedges' g.
#' @param conf_level
#'
#' @export
calc_rm_anova <- function(
    
  data,
  rm = "within",
  value = "value",
  id,
  within_vars,
  between_vars = NULL,
  covariates = NULL,
  interactions = FALSE,
  correction = "auto",
  posthoc_correction = "tukey",
  es_type = "hedges",
  conf_level = 0.95,
  emm_weights = TRUE
  
) {
  

  # MESSAGES & ERRORS -------------------------------------------------------

  if (length(posthoc_correction) > 1) {
    
    stop(
      "More than 1 post-hoc correction method detected.
      Please specify only one post-hoc correction method."
    )
    
  }
  
  
  # RM-ANOVA PARAMETERS -----------------------------------------------------
  
  # Create working data frame
  wdf <- data %>% 
    dplyr::select(
      dplyr::all_of(c(
        id,
        within_vars,
        between_vars,
        covariates)
      )
    )
  
  # Transform into long data
  ldf <- wdf %>%
    
    # Convert to long format
    tidyr::gather(
      key = !!rm,
      value = !!value,
      -all_of(
        c(id,
          between_vars,
          covariates)
      )
    )
  
  
  # Repeated Measures factors & cells
  rmCells <- lapply(
    within_vars,
    function(w) {
      
      wlist <- list(
        measure = w,
        cell = w
      )
      
      return(wlist)
      
    }
  )
  
  
  # Specify interaction terms if any
  if (isFALSE(interactions)) {
    
    if (isFALSE(interactions) && is.null(between_vars) && is.null(covariates)) {
      
      # Define BS ANOVA formula
      bsTerms <- NULL
      
    } else {
      
      # Define BS ANOVA formula
      bsTerms <- as.formula(
        paste0(" ~ ",
               paste0(c(between_vars,
                        covariates),
                      collapse = " + ")))
      
    }
    
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
    
    # Define ANOVA formula
    bsTerms <- as.formula(
      paste0(" ~ ",
             paste0(c(between_vars,
                      covariates,
                      interactions),
                    collapse = " + ")))
    
  }
  
  
  # List of post-hoc effects
  ph_list <- c(rm, between_vars)
  
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
  
  # Repeated measures terms
  rmTerms <- as.formula(
    paste0(
      "~ ",
      rm
    )
  )
  
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
  
  # Run Repeated Measures ANOVA
  aov <- jmv::anovaRM(
    data = data,
    rm = list(
      list(
        label = rm,
        levels = within_vars)),
    rmCells = rmCells,
    bs = vars(!!!bs),
    cov = !!covariates,
    effectSize = c("ges",
                   "eta",
                   "partEta"),
    rmTerms = rmTerms,
    bsTerms = bsTerms,
    spherTests = TRUE,
    spherCorr = if (correction == "auto") {"none"} else {correction},
    leveneTest = TRUE,
    postHoc = postHoc,
    postHocCorr = posthoc_correction,
    emMeans = emMeans,
    emmPlots = FALSE,
    emmTables = TRUE,
    emmWeights = emm_weights,
    ciWidthEmm = conf_level * 100
  )
  
  # Flag if Sphericity Assumption is violated
  if (correction == "auto" && aov[["assump"]][["spherTable"]][["asDF"]][["p"]] < (1-conf_level)) {
    
    # Give sphericity message
    message(
      paste0(
        "Current model indicates a violation of sphericity (Mauchly's W = ",
        round(aov[["assump"]][["spherTable"]][["asDF"]][["mauch"]], 3),
        ", p = ",
        round(aov[["assump"]][["spherTable"]][["asDF"]][["p"]], 3),
        "). ",
        
        # If GG epsilon is less than 0.75, automatically switch to GG correction
        if (aov[["assump"]][["spherTable"]][["asDF"]][["gg"]] < 0.75) {
          
          "Estimated \u03B5 < 0.75, switching to Greenhouse-Geisser correction."
          
        } else {
          
          "Estimated \u03B5 >= 0.75, switching to Huynh-Feldt correction."
          
        }
        
      )
    )
    
    correction <- if (aov[["assump"]][["spherTable"]][["asDF"]][["gg"]] < 0.75) {
     
      "GG"
       
    } else {
      
      "HF"
      
    }
    
    # Rerun Repeated Measures ANOVA
    aov <- jmv::anovaRM(
      data = data,
      rm = list(
        list(
          label = rm,
          levels = within_vars)),
      rmCells = rmCells,
      bs = vars(!!!bs),
      cov = !!covariates,
      effectSize = c("ges",
                     "eta",
                     "partEta"),
      rmTerms = rmTerms,
      bsTerms = bsTerms,
      spherTests = TRUE,
      spherCorr = correction,
      leveneTest = TRUE,
      postHoc = postHoc,
      postHocCorr = posthoc_correction,
      emMeans = emMeans,
      emmPlots = FALSE,
      emmTables = TRUE,
      emmWeights = emm_weights
    )
    
  }
  
  
  # ANOVA SUMMARY -----------------------------------------------------------
  
  # Within-Subjects Summary Table
  rm_sum <- aov[["rmTable"]][["asDF"]] %>% 
    
    # Remove column affixes
    magrittr::set_colnames(
      ., gsub(
        case_when(
          correction == "none" ~ "\\[none\\]",
          correction == "GG" ~ "\\[GG\\]",
          correction == "HF" ~ "\\[HF\\]"
        ), 
        "", 
        colnames(.)
      )
    ) %>% 
    
    # Calculate Cohen's f
    dplyr::mutate(
      cohen_f = sqrt(partEta / (1 - partEta))
    ) %>% 
    
    # Rename
    dplyr::rename(
      etaSq = eta,
      etaSqP = partEta
    )
  
  # Remove rownames
  rownames(rm_sum) <- NULL
  
  
  # If Correction is applied, remove the correction column
  if (correction != "none") {
    
    # Remove correction column
    rm_sum <- dplyr::select(
      rm_sum,
      -correction
    )
    
  }
  
  
  # Between-Subjects Summary Table
  bs_sum <- aov[["bsTable"]][["asDF"]] %>% 
    
    # Calculate Cohen's f
    dplyr::mutate(
      cohen_f = sqrt(partEta / (1 - partEta))
    ) %>% 
    
    # Rename
    dplyr::rename(
      etaSq = eta,
      etaSqP = partEta
    )
  
  # Remove rownames
  rownames(bs_sum) <- NULL
  
  
  
  # ASSUMPTIONS -------------------------------------------------------------
  
  # -- TEST OF SPHERICITY -- #
  rm_sph <- aov[["assump"]][[1]][["asDF"]] %>% 
    
    # Rename
    dplyr::rename(outcome = name)
  
  rownames(rm_sph) <- NULL
  
  # -- HOMOGENEITY OF VARIANCES TEST (LEVENE'S) -- #
  rm_var <- aov[["assump"]][[2]][["asDF"]] %>% 
    
    # Rename
    dplyr::rename(outcome = name)
  
  rownames(rm_var) <- NULL
  
  
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
                        dat <- ldf
                        
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
      
      # Identify if paired
      if (rm %in% postHoc[[i]]) {
        
        # Separate just group combinations for group 1
        ph_g1 <- aov[["postHoc"]][[i]][["asDF"]] %>% 
          select(dplyr::all_of(1:ph_len)) %>% 
          magrittr::set_colnames(
            .,
            gsub("1", "", colnames(.))
          )
        
        # Separate just group combinations for group 2
        ph_g2 <- aov[["postHoc"]][[i]][["asDF"]] %>% 
          select(dplyr::all_of((ph_len+2):(ph_len*2+1))) %>% 
          magrittr::set_colnames(
            .,
            gsub("2", "", colnames(.))
          )
        
        # Specify paired logics to use as flags
        ph_bsVars <- setdiff(names(ph_g1), rm)
        
        # Get paired logic to flag if the contrast is between or within subjects
        ph_paired <- as.data.frame(ph_g1 == ph_g2) %>% 
          dplyr::rowwise() %>%
          dplyr::mutate(paired = (!get(rm) & all(dplyr::c_across(dplyr::all_of(!!ph_bsVars))))) %>% 
          dplyr::pull(paired)
        
      } else {
        
        ph_paired <- FALSE
        
      }
      
      
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
        dplyr::inner_join(ph_desc_g2, by = "group_2") %>% 
        
        # Add paired flag
        dplyr::mutate(
          paired = ph_paired
        )
      
      
      # Get vector of group combinations for every respondent
      int_combos <- do.call(base::interaction,
                            c(ldf[ph_eff],
                              sep = " "))
      
      # Create working dataframe
      wrk_df <- data.frame(
        
        group = int_combos,
        outcome = ldf[[value]]
        
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
          
          # Flag for paired comparison
          paired <- ph_table[[i, "paired"]]
          
          
          # MSE for between subjects
          mse <- if (isTRUE(paired)) {
            
            # If indeed paired, then get MSE from repeated measures residuals
            subset(rm_sum,
                   name == "Residual")[["ms"]]
            
          } else {
            
            # If not paired, then get MSE from between subjects residuals
            subset(bs_sum,
                   name == "Residual")[["ms"]]
            
          }
          
          
          # Calculate RMSE
          rmse <- sqrt(mse)
          
          
          # If paired is TRUE, calculate correlation coefficient r
          r <- if (isTRUE(paired)) {
            
            cor(g1[["outcome"]],
                g2[["outcome"]])
            
          } else {
            
            NULL
            
          }
          
          
          # Calculate effect size
          es <- calc_esm(
            
            mean = c(m1, m2),
            sd = c(sd1, sd2),
            n = c(n1, n2),
            sd_pooled = rmse,
            r = r,
            paired = paired,
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
      is.null(between_vars) ~ "Repeated Measures",
      !is.null(between_vars) ~ "Mixed Model"
    ),
    rm = rm,
    value = value,
    id = id,
    within_vars = within_vars,
    between_vars = between_vars,
    covariates = covariates,
    interactions = interactions,
    correction = correction,
    es_type = es_type,
    conf_level = conf_level
    
  )
  
  # COMPILE INTO RESULTS ----------------------------------------------------
  
  # Compile results
  return_list <- list(
    
    bs_summary = bs_sum,
    ws_summary = rm_sum,
    rm_sph = rm_sph,
    rm_var = rm_var,
    emm = emm_table,
    emmc = emmc_table,
    meta_data = meta_data
    
  )
  
  # Return
  return(return_list)
  
}
