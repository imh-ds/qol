#' Calculate ANOVA Estimated Marginal Means
#'
#' @param data A dataframe object. This should be a structured dataset where
#'   each column represents a variable and each row represents an observation.
#' @param outcome Character indicating the outcome/dependent variable.
#' @param groups Character indicating the factor grouping variable.
#' @param between_vars An optional character indicating the between groups
#'   factor grouping variable.
#' @param within_vars Characters indicating the repeated measures variables.
#' @param covariates An optional vector of variable names representing
#'   covariates of interest.
#' @param aov_result The ANOVA model specified via \code{stats::aov()}.
#' @param aov_summary ANOVA summary dataframe created from
#'   \code{calc_aov_summary()}.
#' @param conf_level Numeric value reflecting the confidence interval level. The
#'   default is 0.95 for 95% confidence interval.
#' @param ph_method The post-hoc pairwise comparison correction method. Options
#'   include \code{"tukey"}, \code{"scheffe"} , \code{"sidak"},
#'   \code{"bonferroni"}, \code{"dunnettx"}, \code{"mvt"}, and \code{"none"}.
#'   The default is \code{"tukey"}. See below for additional information.
#' @param paired A logical reflecting whether the pairwise comparison is between
#'   (FALSE) or within (TRUE) subjects. If within, then the pair is tested
#'   whether it is truly within by iterating through the unique between subjects
#'   combinations. \code{bs_unique} must be specified.
#'
#' @export
calc_aov_emm <- function(
    
  data,
  groups,
  outcome,
  between_vars = NULL,
  within_vars = NULL,
  covariates = NULL,
  aov_result,
  aov_summary,
  conf_level,
  ph_method,
  es_type,
  g_correction,
  paired = FALSE
  
) {
  
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
        dplyr::filter(dplyr::if_all(dplyr::all_of(int_groups), ~ !is.na(.))) %>% 
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
          
          # MSE
          if (!isFALSE(paired)) {
            
            # For between subjects
            if (paired == "between") {
              mse <- summary(aov_result)[[1]][[1]]["Residuals", "Mean Sq"]
            }
            
            # For within subjects
            if (paired == "within") {
              mse <- summary(aov_result)[[2]][[1]]["Residuals", "Mean Sq"]
            }
            
          } else {
            
            mse <- summary(aov_result)[[1]]["Residuals", "Mean Sq"]
            
          }
          
          
          # Calculate RMSE
          rmse <- sqrt(mse)
          
          # Calculate effect size
          es <- calc_es_d(
            
            outcome = pc_df[["outcome"]],
            group = pc_df[["group"]],
            sd_pooled = rmse,
            es_type = es_type,
            g_correction = g_correction,
            conf_level = conf_level,
            paired = if(paired == "within") {TRUE} else {FALSE},
            bs_unique = if(paired == "within" && 
                           (!is.null(between_vars) |
                            !is.null(covariates))) {unique(
              data[c(between_vars,
                     covariates)]
            )} else {NULL},
            within_vars = within_vars
            
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
  
  # Return
  return(emm_list)
  
}
