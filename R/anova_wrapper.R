#' Analysis of Variance (ANOVA) Wrapper
#'
#' @description A wrapper for running Analysis of Variance (ANOVA) with or
#' without covariates.
#'
#' @param data A dataframe object. This should be a structured dataset where
#'   each column represents a variable and each row represents an observation.
#' @param outcome Character indicating the outcome/dependent variable.
#' @param groups Character indicating the factor grouping variable.
#' @param covariates An optional vector of variable names representing
#'   covariates of interest.
#' @param interaction Either a logical or vector of characters representing the
#'   interaction effects of interest. If specified as \code{TRUE}, all possible
#'   interactions between the groups are run (not recommended). If specified as
#'   \code{FALSE}, no interaction terms are run. If specified with interaction
#'   terms, e.g., \code{c("treatment\*gender", "treatment\*age_group",
#'   "treatment\*gender\*age_group")}, the only the specified interaction terms
#'   are run. The default is \code{FALSE}.
#' @param ph_method The post-hoc pairwise comparison correction method. Options
#'   include \code{"tukey"}, \code{"scheffe"} , \code{"sidak"},
#'   \code{"bonferroni"}, \code{"dunnettx"}, \code{"mvt"}, and \code{"none"}.
#'   The default is \code{"tukey"}. See below for additional information.
#' @param conf_level Numeric value reflecting the confidence interval level. The
#'   default is 0.95 for 95% confidence interval.
#' @param es_type Character indicating which effect size to return. Options
#'   include \code{"cohen"} for Cohen's d, \code{"hedges"} for Hedges' g, or
#'   \code{"glass"} for Glass's delta. The default is Hedges' g.
#' @param g_correction Character indicating which Hedges' g correction to apply.
#'   Options include \code{"standard"} for regular Hedges' g correction and
#'   \code{"log"} to apply logarithmic Hedges' g correction. The default is
#'   standard.
#'
#' @return
#'
#' @examples
#'
#' @export
wrap_anova <- function(
    
  data,
  outcome,
  groups,
  covariates = NULL,
  interaction = FALSE,
  ph_method = "tukey",
  conf_level = .95,
  es_type = "hedges",
  g_correction = "standard"
  
) {
  
  
  # WARNINGS & MESSAGES -----------------------------------------------------
  
  # If covariates is specified
  if (!is.null(covariates)) {
    
    message(
      "Detecting covariates. Running ANCOVA."
    )
    
  }
  
  # If interaction is TRUE
  if (isTRUE(interaction)) {
    
    message(
      paste(
        "Exact interaction terms not specified.",
        "Running interactions across all groups."
      )
    )
    
  }
  
  
  
  # BASIC DATA PARAMETERS ---------------------------------------------------
  
  
  # Change groups to factors
  for(g in groups){
    
    # Automatically detect if variable has only numerics. If so, recode to add
    # "L" as a prefix for "Level".
    if (all(grepl("^\\d+$", 
                  data[[g]], 
                  perl = TRUE) | is.na(data[[g]]))) {
      
      # Add "L" in front
      data[[g]] <- ifelse(!is.na(data[[g]]), 
                          paste0("L", 
                                 data[[g]]), 
                          data[[g]])
      
    }
    
    # If the group variable has spaces, recode to underscore "_".
    if (any(grepl(" ",
                  data[[g]],
                  fixed = TRUE))) {
      
      data[[g]] <- gsub(" ",
                        "_",
                        data[[g]])
      
    }
    
    # If not already a factor, then factorize the group variables.
    if (!is.factor(data[[g]])) {
      
      data[[g]] <- as.factor(data[[g]])
      
    }
    
  }
  
  
  # Specify interaction terms if any
  if (isFALSE(interaction)) {
    
    # Define ANOVA formula
    formula <- as.formula(
      paste0(outcome,
             " ~ ",
             paste0(c(groups,
                      covariates),
                    collapse = " + ")))
    
    # Define EMM formula
    em_formula <- as.formula(
      paste0(" ~ ",
             paste0(c(groups,
                      covariates),
                    collapse = " + ")))
    
  } else {
    
    # If interaction is just specified as TRUE instead of defining specific
    # interaction, then automate every interaction across groups
    if (isTRUE(interaction)) {
      
      interactions_list <- lapply(
        2:length(groups),
        function(n) {
          
          combinations <- combn(groups, n)
          
          result <- apply(combinations,
                          2,
                          function(x) paste(x, collapse = "*"))
          
        }
        
      )
      
      int_combos <- purrr::reduce(interactions_list,
                                  c)
      
    }
    
    # Define ANOVA formula
    formula <- as.formula(
      paste0(outcome,
             " ~ ",
             paste0(c(groups,
                      covariates,
                      int_combos),
                    collapse = " + ")))
    
    # Define EMM formula
    em_formula <- as.formula(
      paste0(" ~ ",
             paste0(c(groups,
                      covariates,
                      int_combos),
                    collapse = " + ")))
    
  }
  
  
  
  # RUN ANOVA ---------------------------------------------------------------
  
  # Perform the ANOVA
  aov_result <- aov(formula = formula, 
                    data = data)
  
  
  
  # RUN ASSUMPTION TESTS ----------------------------------------------------
  
  
  # -- NORMALITY ASSUMPTION TEST -- #
  
  # Grab data with no NA
  nt_var <- data[complete.cases(data[[outcome]]),] %>% 
    pull(outcome)
  
  # Grab residuals (errors) of ANOVA
  nt_res <- residuals(aov_result)
  
  
  # Shapiro-Wilk Normality Test
  sw_var <- shapiro.test(nt_var)
  sw_res <- shapiro.test(nt_res)
  
  # Kolmogorov-Smirnov Test Generally, Shapiro-Wilk and Anderson-Darling
  # preferred for behavioral science research due to KS assuming all values in
  # the data are unique. In BeSci research, the use of self-report survey
  # measures often mean a lot of tied values (i.e., duplicates). Thus, the high
  # degree of tied values will likely affect the shape of the empirical
  # distribution function (EDF) of the sample when compared against the
  # cumulative distribution function (CDF) of the specified theoretical
  # distribution, such as a normal distribution. The p-values may therefore be
  # inaccurate.
  ks_var <- suppressWarnings(
    ks.test(nt_var,
            "pnorm",
            mean = mean(nt_var),
            sd = sd(nt_var),
            alternative = "two.sided")
  )
  ks_res <- suppressWarnings(
    ks.test(nt_res,
            "pnorm",
            mean = mean(nt_res),
            sd = sd(nt_res))
  )
  
  # Anderson-Darling Test
  ad_var <- nortest::ad.test(nt_var)
  ad_res <- nortest::ad.test(nt_res)
  
  
  # Compile Normality-Test Assumptions
  normal_test <- data.frame(
    type = c("Variable", rep(NA, 2),
             "Residual Errors", rep(NA, 2)),
    method = rep(c("Shapiro-Wilk",
                   "Kolmogorov-Smirnov",
                   "Anderson-Darling"), 2),
    test = rep(c("W",
                 "D",
                 "A"), 2),
    statistic = c(sw_var[["statistic"]],
                  ks_var[["statistic"]],
                  ad_var[["statistic"]],
                  sw_res[["statistic"]],
                  ks_res[["statistic"]],
                  ad_res[["statistic"]]),
    p = c(sw_var[["p.value"]],
          ks_var[["p.value"]],
          ad_var[["p.value"]],
          sw_res[["p.value"]],
          ks_res[["p.value"]],
          ad_res[["p.value"]])
  )
  
  
  
  # -- HOMOGENEITY OF VARIANCE TESTS -- #
  
  # Bartlett Test
  bt_formula <- as.formula(paste0(outcome,
                                  " ~ interaction(",
                                  paste0(groups,
                                         collapse = ", "),
                                  ")"))
  bt <- bartlett.test(formula = bt_formula,
                      data = data)
  
  
  # Levene Test
  lt_formula <- as.formula(paste0(outcome,
                                  " ~ ",
                                  paste0(groups,
                                         collapse = "*")))
  lt <- car::leveneTest(y = lt_formula,
                        data = data,
                        center = "mean")
  
  
  # Compile Homogeneity of Variance Test Assumptions
  hvar_test <- data.frame(
    
    method = c("Levene's",
               "Bartlett's"),
    test = c("F",
             "\u03C7\u00B2"),
    statistic = c(lt[["F value"]][[1]],
                  bt[["statistic"]][[1]]),
    df1 = c(lt[["Df"]][[1]],
            bt[["parameter"]][[1]]),
    df2 = c(lt[["Df"]][[2]],
            NA),
    p = c(lt[["Pr(>F)"]][[1]],
          bt[["p.value"]])
    
  )
  
  
  
  # COMPILE SUMMARY OF ANOVA RESULTS ----------------------------------------
  
  
  # Effect size for ANOVA
  aov_summary <- tibble::as_tibble(
    sjstats::anova_stats(
      aov_result,
      digits = 22
    )
  ) %>% 
    
    # Rename variables
    dplyr::rename(
      
      effect = term,
      f_value = statistic,
      p_value = p.value,
      etasq_partial = partial.etasq,
      omegasq_partial = partial.omegasq,
      cohens_f = cohens.f
      
    ) %>% 
    
    # Reorder
    dplyr::select(
      
      effect,
      df,
      sumsq,
      meansq,
      f_value,
      p_value,
      dplyr::everything()
      
    ) %>% 
    
    # Recode ":" into "*"
    dplyr::mutate(
      
      effect = gsub(":",
                    "*",
                    effect)
      
    )
  
  # Overall Model
  residual_stats <- aov_summary %>% 
    
    # Filter to just the Residuals row
    dplyr::filter(effect == "Residuals") %>% 
    
    # Grab just the df and MeanSq values
    dplyr::summarize(df = df,
                     meansq = meansq) %>% 
    
    # Pull
    base::unlist()
  
  overall_model <- aov_summary %>% 
    
    # Filter to everything but the Residuals
    dplyr::filter(effect != "Residuals") %>% 
    
    # Select the df and SumSq
    dplyr::select(df, sumsq) %>% 
    
    # Summarize
    dplyr::summarise(dplyr::across(.cols = dplyr::everything(),
                                   \(x) sum(x, na.rm = T))) %>% 
    
    # Mutate new values
    dplyr::mutate(effect = "Overall Model",
                  meansq = sumsq / df,
                  f_value = meansq / residual_stats[["meansq"]],
                  p_value = 1 - pf(f_value, df, residual_stats[["df"]])) %>% 
    
    # Relocate the effect to the first column
    dplyr::relocate(effect,
                    .before = df)
  
  
  # Bind rows of the ANOVA summary with the Overall Model statistics
  aov_summary <- overall_model %>% 
    dplyr::bind_rows(aov_summary)
  
  
  
  
  # ESTIMATED MARGINAL MEANS ------------------------------------------------
  
  
  # Grab post-hoc interaction effects to use in estimated marginal means
  ph_interaction_effects <- aov_summary[grepl("\\*",
                                              aov_summary[["effect"]]), ] %>% 
    pull(effect)
  
  
  # Get vector of all main and interaction effects
  effects_list <- c(groups,
                    ph_interaction_effects)
  
  # Get main and simple effects
  emm_list <- lapply(
    
    seq_along(effects_list),
    function(e) {
      
      # Get EMM name
      emm_name <- effects_list[[e]]
      
      # Specify formula for EMM
      emm_formula <- as.formula(
        paste0(
          "~ ",
          effects_list[[e]]
        )
      )
      
      # Get EMM
      emm <- suppressMessages(
        emmeans::emmeans(
          object = aov_result,
          specs = emm_formula,
          level = conf_level
        )
      )
      
      # Correct EMM dataframe
      emm_df <- tibble::as_tibble(emm) %>% 
        
        # Rename
        dplyr::rename(
          
          mean = emmean,
          se = SE,
          ci_lower = lower.CL,
          ci_upper = upper.CL
          
        )
      
      # Get pairwise contrast
      emm_contrast <- emmeans::contrast(
        emm,
        method = "pairwise",
        adjust = ph_method,
        level = conf_level
      )
      
      # Correct EMM Contrast dataframe
      emmc_df <- tibble::as_tibble(emm_contrast) %>% 
        
        # Separate comparison into separate group columns
        tidyr::separate(contrast,
                        into = c("group_1",
                                 "group_2"),
                        sep = " - ") %>% 
        
        # Rename
        dplyr::rename(
          
          m_diff = estimate,
          se = SE,
          t = t.ratio,
          p_value = p.value
          
        )
      
      # ---- IF INTERACTION ---- #
      
      # Interacting groups
      int_groups <- strsplit(emm_name,
                             split = "\\*")[[1]]
      
      
      # Get sample size (n) for EMM
      n_df <- as_tibble(data) %>% 
        dplyr::filter(dplyr::if_all(int_groups, ~ !is.na(.))) %>% 
        dplyr::group_by(dplyr::across(dplyr::all_of(int_groups))) %>% 
        dplyr::summarize(n = n(),
                         .groups = "drop")
      
      
      # Add sample size (n) to EMM
      emm_df <- suppressMessages(
        
        # Merge
        dplyr::inner_join(
          emm_df,
          n_df
        ) %>% 
          
          # Reorder
          dplyr::relocate(
            n,
            .before = ci_lower
          )
      )
      
      # Possible combinations
      int_combos <- do.call(base::interaction,
                            c(data[int_groups],
                              sep = " "))
      
      # Create working dataframe
      wrk_df <- data.frame(
        
        group = int_combos,
        outcome = data[[outcome]]
        
      )
      
      # Identify row to iterate
      eff_desc <- lapply(
        seq(nrow(emmc_df)),
        function(i) {
          
          # Get names of groups 1 & 2
          group1 <- as.character(emmc_df[i,][["group_1"]])
          group2 <- as.character(emmc_df[i,][["group_2"]])
          
          # Identify pair
          pair <- c(group1,
                    group2)
          
          # Apply filter to pairwise comparison groups
          pc_df <- wrk_df %>% 
            dplyr::filter(sapply(pair,
                                 grepl,
                                 group) %>%
                            apply(1, any))
          
          # Outcome data
          out_df <- as.numeric(pc_df[["outcome"]])
          grp_df <- as.character(pc_df[["group"]])
          
          # Get group standard deviations
          grp_sd <- tapply(out_df,
                           grp_df,
                           sd,
                           na.rm = T)
          
          # Get group means
          grp_m <- tapply(out_df,
                          grp_df,
                          mean,
                          na.rm = T)
          
          # Get group n
          grp_n <- tapply(out_df,
                          grp_df,
                          \(x) length(na.omit(x)))
          
          # MSE
          mse <- summary(aov_result)[[1]]["Residuals", "Mean Sq"]
          
          # Calculate RMSE
          rmse <- sqrt(mse)
          
          # Calculate effect size
          es <- calc_es_d(
            
            g1_m = grp_m[[1]],
            g2_m = grp_m[[2]],
            g1_sd = grp_sd[[1]],
            g2_sd = grp_sd[[2]],
            g1_n = grp_n[[1]],
            g2_n = grp_n[[2]],
            sd_pooled = rmse,
            es_type = es_type,
            g_correction = g_correction,
            conf_level = conf_level
            
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
        
        inner_join(emmc_df,
                   eff_table)
        
      )
      
      # If iterating on interaction effect, also get interaction contrast
      if (emm_name %in% ph_interaction_effects) {
        
        # Get interaction contrast
        emm_interaction <- emmeans::contrast(
          emm,
          method = "pairwise",
          interaction = TRUE
        )
        
        # Correct EMM Interaction contrast dataframe
        emmi_df <- tibble::as_tibble(emm_interaction) %>% 
          
          # Rename
          dplyr::rename(
            
            m_diff = estimate,
            se = SE,
            t = t.ratio,
            p_value = p.value
            
          )
        
        # Compile
        emm_sheet <- list(
          emm = emm_df,
          emmc = emmc_table,
          emmi = emmi_df
        )
        
      } else {
        
        # Compile
        emm_sheet <- list(
          emm = emm_df,
          emmc = emmc_table
        )
        
      }
      
      # Return
      return(emm_sheet)
      
    }
    
  )
  
  # Rename
  names(emm_list) <- effects_list
  
  
  
  # COMPILE INTO RETURNABLE RESULTS -----------------------------------------
  
  
  # Compile all results
  aov_results <- c(list(summary = aov_summary,
                        norm = normal_test,
                        var = hvar_test),
                   emm_list)
  
  # Return
  return(aov_results)
  
}