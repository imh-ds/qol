#' Partial Least Squares Structural Equation Modeling Wrapper
#'
#' @description A wrapper function to automate PLS-SEM and extracting its
#'   results. The current function is limited to only 2 serial mediators. Due to
#'   the undue level of complexity added to the model estimations when there are
#'   3 or more serial mediators, the current function is not designed to handle
#'   this type of model. This automation function uses the \code{seminr} package
#'   (Ray et al., 2022).
#'
#' @param data A dataframe object. This should be a structured dataset where
#'   each column represents a variable and each row represents an observation.
#' @param measurements Measurement model object created using \code{constructs}
#'   function from the \code{seminr} package. Refer to the \code{seminr} help
#'   documentation for more details.
#' @param structure Structural model object created using the
#'   \code{relationships} function from \code{seminr} package. Refer to the
#'   \code{seminr} help documentation for more details.
#' @param bootn A numeric value specifying the number of bootstrap iterations to
#'   perform. The default value is \code{1000}. If specified to \code{0}, then
#'   no bootstrapping is performed.
#' @param inner_weights A \code{seminr} parameter to specify the inner weighting
#'   scheme in the estimation of the inner paths matrix. Options include
#'   \code{seminr::path_weighting} (default) or \code{path_factorial}.
#' @param missing A \code{seminr} parameter to specify how the model should
#'   handle missing values. The default is \code{seminr::mean_replacement}.
#' @param missing_value A value indicating the missing value in the dataset. The
#'   default is \code{NA}.
#' @param max_iter A numeric value indicating the maximum number of iterations
#'   to run when estimating PLS-SEM. The default value is \code{300}.
#' @param stop_criterion A numeric value indicating the stop criterion for
#'   estimating PLS-SEM. The default value is \code{7}.
#' @param digits A numeric value indicating the number of digits to round to.
#' @param prioritization A logical value for whether to run feature
#'   prioritization analysis.
#' @param prioritization_type A string value indicating which weighting scheme
#'   to use in calculating the importance of individual features to their
#'   composite variable. Options include \code{"loading"} to weight by indicator
#'   composite loading, \code{"weight"} to weight by indicator composite
#'   weights, or \code{"combo"} to weight by both indicator composite loadings
#'   and weights. The default is \code{"combo"}.
#' @param prioritization_weight_type A string value indicating which weighting
#'   scheme to use in calculating the importance of individual features at the
#'   lower-order level. Options include \code{"se"} to weight by standard error,
#'   \code{"est"} to weight by item loading, or \code{"combo"} to weight by both
#'   standard error and item loading. The default is \code{"combo"}.
#' @param prioritization_cor_method A string value indicating which correlation
#'   coefficient to use in calculating the halo-effect feature weighting.
#'   Options include \code{"pearson"} for Pearson's correlation,
#'   \code{"kendall"} for Kendall rank correlation, and \code{"spearman"} for
#'   Spearman's rank correlation. The default is \code{"pearson"}.
#' @param study_name An optional character string indicating the name of the
#'   study.
#' @param path_tables A vector of strings indicating which direct effects
#'   pathway table to export. Options include \code{"long"} for long-formatted
#'   regression tables with confidence intervals below the coefficients,
#'   \code{"wide"} for wide-formatted regression tables with confidence
#'   intervals adjacent to the coefficients, or \code{NULL} to not export any.
#' @param filename Location to save output file as excel workbook if specified.
#'
#' @examples
#' measurements <- constructs(
#'   composite("comp1", multi_items("com", 1:3)),
#'   composite("comp2", multi_items("com", 4:6)),
#'   higher_composite("high_comp", c("comp1", "comp2")),
#'   composite("mani1", c("v1")),
#'   composite("mani2", c("v2")),
#'   composite("outcome", multi_items("out", 1:5)),
#'   interaction_term(iv = "high_comp", moderator = "mani1", method = two_stage)
#' )
#'
#' structure <- relationships(
#'   paths(from = c("high_comp"),
#'         to = c("mani1", "mani2")),
#'   paths(from = c("high_comp", "mani1", "mani2", "high_comp*mani1"),
#'         to = c("outcome"))
#' )
#'
#' plssem_wrapper(data,
#'                measurements,
#'                structure)
#'
#'
#' @references Ray S, Danks N, Calero Valdez A (2022). seminr: Building and
#'   Estimating Structural Equation Models. R package version 2.3.2,
#'   \url{https://CRAN.R-project.org/package=seminr}.
#'
#' @export
wrap_plssem <- function(
    data = .,
    measurements,
    structure,
    bootn = 1000,
    inner_weights = seminr::path_weighting,
    missing = seminr::mean_replacement,
    missing_value = NA,
    max_iter = 300,
    stop_criterion = 7,
    digits = 3,
    prioritization = FALSE,
    prioritization_type = "combo",
    prioritization_weight_type = "combo",
    prioritization_cor_method = "pearson",
    study_name = NULL,
    path_tables = c("long", "wide"),
    filename = NULL
) {
  

  # EXTRACT STRUCTURE VARIABLES ---------------------------------------------
  
  # Create a dataframe from the structure
  structure_df <- base::data.frame(structure)
  
  # Identify unique variables in the source and target columns
  vars <- base::unique(c(structure_df[["source"]],
                         structure_df[["target"]]))
  
  # Identify predictors and outcomes
  predictors <- vars[!(vars %in% structure_df[["target"]])]
  outcomes <- vars[!(vars %in% structure_df[["source"]])]
  paths <- base::unique(structure_df[["target"]])
  
  # Exclude outcomes and get unique targets for mediator variables
  structure_df_ex <- structure_df[!structure_df[["target"]] %in% outcomes, ]
  
  # Identify mediators and serial mediators
  med_vars <- base::unique(structure_df_ex[["target"]])
  
  # Identify any pathways indicating serial mediators based on whether mediators
  # are predicting mediators
  structure_df_ex <- base::transform(
    
    structure_df_ex,
    serial_flag = source %in% med_vars & target %in% med_vars
    
  )
  
  # Flag if there are any serial mediators; 1 if flagged, 0 if not.
  serial_flag <- base::as.integer(
    base::any(structure_df_ex[["serial_flag"]])
  )
  
  # Run if presence of serial mediators is identified
  if(serial_flag == 1){
    
    # Get second structural dataframe but exclude all predictors and outcomes
    structure_df_ex <- structure_df[!(structure_df[["source"]] %in% predictors) & 
                                      !(structure_df[["target"]] %in% outcomes), ]
    
    # Get vector of unique mediating variables
    med_vars <- base::unique(c(structure_df_ex[["source"]],
                               structure_df_ex[["target"]]))
    
    # Extract mediating and serial mediating variables
    mediators <- med_vars[!(med_vars %in% structure_df_ex[["target"]])]
    serial_mediators <- med_vars[!(med_vars %in% structure_df_ex[["source"]])]
    
  } else {
    
    mediators <- med_vars
    serial_mediators <- NULL
    
  }
  
  # If mediators is length 0, then assign NULL
  if(length(mediators) == 0){
    
    mediators <- NULL
    
  }
  

  # PLS-SEM BASIC ANALYSIS & BOOTSTRAPPING ----------------------------------

  # Run PLS-SEM analysis
  pls_model <- seminr::estimate_pls(
    
    data = data,
    measurement_model = measurements,
    structural_model = structure,
    inner_weights = inner_weights,
    missing = missing,
    missing_value = missing_value,
    maxIt = max_iter,
    stopCriterion = stop_criterion
    
  )
  
  # Get PLS-SEM summary of results
  model_summary <- summary(pls_model)
  
  # Boostrap PLS-SEM to get confidence intervals
  boot_mod <- seminr::bootstrap_model(
    seminr_model = pls_model,
    nboot = bootn
  )
  
  # Get summary statistics
  summarystat <- base::summary(boot_mod) #Create object
  

  # EXTRACT SUMMARY RESULTS -------------------------------------------------

  # -- PATH SUMMARY -- #
  
  # Get general model path summary
  modelsum <- base::data.frame(model_summary[["paths"]]) %>%
    tibble::rownames_to_column(var = "variable")
  
  modelsum_rsq <- modelsum %>% 
    dplyr::filter(base::grepl("R\\^2", variable)) %>% 
    dplyr::mutate(variable = base::gsub("AdjR\\^2", "R\u00B2 Adj", variable),
                  variable = base::gsub("R\\^2", "R\u00B2", variable))
  
  modelsum_coef <- modelsum %>% 
    dplyr::filter(!base::grepl("R\\^2", variable)) %>% 
    dplyr::mutate(variable = base::gsub("\\*", " \u00D7 ", variable))
  
  modelsum <- rbind(modelsum_coef,
                    modelsum_rsq)
  
  
  
  # -- Fsq EFFECT SIZES -- # 
  
  # Get model effect sizes
  modeleffsize <- base::data.frame(model_summary[["fSquare"]]) %>%
    tibble::rownames_to_column(var = "variable") %>% 
    dplyr::select_if(~base::any(. != 0)) %>% 
    dplyr::mutate(variable = base::gsub("\\*",
                                        " \u00D7 ",
                                        variable)) %>% 
    replace_with_na(vars = dplyr::everything(),
                    values = 0)
  
  
  
  # -- MEASUREMENT RELIABILITY -- #
  
  # Get model reliability statistics
  modelreliability <- base::data.frame(model_summary[["reliability"]]) %>%
    tibble::rownames_to_column(var = "variable") %>% 
    # Remove interaction effects from reliability
    dplyr::filter(!stringr::str_detect(variable, pattern = "\\*"))
  
  # Get composite scores from the PLS SEM
  model_cscores <- base::as.data.frame(pls_model[["construct_scores"]])
  
  # Get composite scores but with interaction effects removed
  model_cs_noint <- model_cscores %>% 
    dplyr::select(-dplyr::contains("*"))
  
  # Get correlation matrix of the composite scores
  cor_cscores <- stats::cor(model_cs_noint)
  
  # Turn diagonal into NA
  base::diag(cor_cscores) <- NA
  
  # Get max of each column in correlation matrix
  cor_max <- base::apply(cor_cscores, 2, max,
                   na.rm = T)
  
  # Get max correlation as data frame
  cor_df <- base::as.data.frame(cor_max) %>% 
    tibble::rownames_to_column(var = "variable")
  
  # Combine with model reliability
  modelreliability <- base::suppressMessages(
    modelreliability %>% 
      dplyr::full_join(cor_df) %>% 
      dplyr::relocate(rhoA, .after = rhoC)
  )
  
  
  
  # -- MEASUREMENT VALIDITY -- #
  
  modelhtmt <- base::as.data.frame(summarystat[["bootstrapped_HTMT"]]) %>% 
    dplyr::rename(
      
      est = "Original Est.",
      boot_est = "Bootstrap Mean",
      boot_se = "Bootstrap SD",
      t = "T Stat.",
      lower_ci = "2.5% CI",
      upper_ci = "97.5% CI"
      
    ) %>% 
    tibble::rownames_to_column(var = "path") %>% 
    dplyr::mutate(path = base::gsub("\\*", " \u00D7 ", path))
  
  # Define all variables
  mod_vars <- c(predictors,
                mediators,
                serial_mediators,
                outcomes)
  
  # Simplify to non-interaction composites
  htmt_df <- modelhtmt %>% 
    dplyr::filter(!stringr::str_detect(path,
                                       pattern = " \u00D7 ")) %>% 
    dplyr::mutate(var1 = base::sapply(stringr::str_split(path, "  ->  "),
                                      `[`, 1),
                  var2 = base::sapply(stringr::str_split(path, "  ->  "),
                                      `[`, 2)) %>% 
    dplyr::select(path, var1, var2, est) %>% 
    dplyr::filter((var1 %in% mod_vars) & (var2 %in% mod_vars)) %>% 
    tidyr::gather(key = "var",
                  value = "variable",
                  var1,
                  var2) %>% 
    dplyr::group_by(variable) %>% 
    dplyr::summarise(htmt = max(est))
  
  # Combine with reliability data frame
  modelrelval <- base::suppressMessages(
    modelreliability %>% 
      dplyr::full_join(htmt_df) %>% 
      dplyr::mutate(
        sqrt_AVE = base::sqrt(AVE),
        valid_AVE = base::ifelse(sqrt_AVE > cor_max,
                                 "PASS",
                                 "FAIL"),
        valid_HTMT = base::ifelse(htmt < .9,
                                  "PASS",
                                  "FAIL"))
  )
  
  
  
  # -- VALIDITY - FRONELL-LARCKER CRITERION -- #
  
  # Get data frame of FL criterion matrix
  fl_val_df <- base::as.data.frame(
    model_summary[["validity"]][["fl_criteria"]]
  ) %>% 
    tibble::rownames_to_column(var = "variable")
  
  # Filter out rows & columns
  fl_val <- fl_val_df %>% 
    
    # Filter rows
    dplyr::filter(
      
      # Remove interaction effects
      !stringr::str_detect(variable, fixed("*")),
      
      # Remove lower-order constructs
      base::sapply(c(predictors,
                     mediators,
                     serial_mediators,
                     outcomes),
                   function(x) stringr::str_detect(variable,
                                                   x)) %>% 
        base::apply(1, any)
      
    ) %>% 
    
    # Filter columns
    dplyr::select(
      variable,
      
      # Remove lower-order constructs
      dplyr::matches(base::paste(
        c(predictors,
          mediators,
          serial_mediators,
          outcomes), 
        collapse = "|"))
      
    ) %>% 
    
    dplyr::select(
      
      # Remove interaction effects
      !dplyr::contains("*")
      
    )
  
  
  
  # -- VALIDITY - HETEROTRAIT MONOTRAIT RATIO -- #
  
  # Get data frame of FL criterion matrix
  htmt_val_df <- as.data.frame(
    model_summary[["validity"]][["htmt"]]
  ) %>% 
    tibble::rownames_to_column(var = "variable")
  
  # Filter out rows & columns
  htmt_val <- htmt_val_df %>% 
    
    # Filter rows
    dplyr::filter(
      
      # Remove interaction effects
      !stringr::str_detect(variable, fixed("*")),
      
      # Remove lower-order constructs
      base::sapply(c(predictors,
                     mediators,
                     serial_mediators,
                     outcomes),
                   function(x) stringr::str_detect(variable,
                                                   x)) %>% 
        base::apply(1, any)
      
    ) %>% 
    
    # Filter columns
    dplyr::select(
      variable,
      
      # Remove lower-order constructs
      dplyr::matches(base::paste(
        c(predictors,
          mediators,
          serial_mediators,
          outcomes), 
        collapse = "|"))
      
    ) %>% 
    
    dplyr::select(
      
      # Remove interaction effects
      !dplyr::contains("*")
      
    )
  
  
  
  # -- MODEL VARIATION INFLATION FACTOR -- #
  
  # Loop and assign VIF statistics 
  viftables <- lapply(paths,
                      function(p) {
                        
                        df <- data.frame(model_summary[["vif_antecedents"]][[p]])
                        df <- setNames(df, p)
                        df <- tibble::rownames_to_column(df,
                                                         var = "variable")
                        
                        return(df)
                        
                      })
  
  # Collapse VIF stats together into table
  modelvif <- suppressMessages(
    purrr::reduce(viftables,
                  dplyr::full_join)) %>% 
    dplyr::mutate(variable = gsub("\\*", 
                                  " \u00D7 ", 
                                  variable))
  
  
  
  # -- MEASUREMENT LOADINGS -- #
  
  # Create loadings dataframe
  modelloadings <- as.data.frame(summarystat[["bootstrapped_loadings"]]) %>%
    dplyr::rename(
      est = "Original Est.",
      boot_est = "Bootstrap Mean",
      boot_se = "Bootstrap SD",
      t = "T Stat.",
      lower_ci = "2.5% CI",
      upper_ci = "97.5% CI") %>% 
    tibble::rownames_to_column(var = "path") %>%
    dplyr::mutate(z = boot_est / boot_se,
                  p = 2*pnorm(-abs(z)),
                  path = gsub("\\*", 
                              " \u00D7 ", 
                              path)) %>% 
    # Relocate Z
    dplyr::relocate(c(z),
                    .after = t)
  
  # Extract composite variables
  comp_vars <- modelloadings %>% 
    dplyr::filter(!stringr::str_detect(path,
                                       pattern = " \u00D7 ")) %>% 
    dplyr::mutate(comp_vars = sapply(stringr::str_split(path,
                                                        "  ->  "),
                                     `[`,
                                     2)) %>% 
    dplyr::pull(comp_vars) %>% 
    base::unique()
  
  # Create list of composite loadings
  mod_loadings_list <- lapply(
    comp_vars,
    function(v) {
      
      ldf <- modelloadings %>% 
        dplyr::filter(!stringr::str_detect(path,
                                           pattern = " \u00D7 "),
                      stringr::str_detect(path,
                                          pattern = paste0(v,"$"))) %>% 
        dplyr::select(path, boot_est) %>% 
        dplyr::rename(!!sym(v) := boot_est)
      
      return(ldf)
      
    }
  )
  
  # Reduce loadings into single data frame
  mod_loadings <- suppressMessages(
    purrr::reduce(
      mod_loadings_list,
      dplyr::full_join)) %>% 
    dplyr::mutate(variable = sapply(stringr::str_split(path,
                                                       "  ->  "),
                                    `[`,
                                    1)) %>% 
    dplyr::select(-path) %>% 
    dplyr::select(variable,
                  dplyr::everything())
  
  
  
  # -- MEASUREMENT WEIGHTS -- #
  
  # Create weights dataframe
  modelweights <- as.data.frame(summarystat[["bootstrapped_weights"]]) %>%
    dplyr::rename(
      est = "Original Est.",
      boot_est = "Bootstrap Mean",
      boot_se = "Bootstrap SD",
      t = "T Stat.",
      lower_ci = "2.5% CI",
      upper_ci = "97.5% CI") %>% 
    tibble::rownames_to_column(var = "path") %>%
    dplyr::mutate(z = boot_est / boot_se,
                  p = 2*pnorm(-abs(z)),
                  path = gsub("\\*", 
                              " \u00D7 ", 
                              path)) %>% 
    # Relocate Z
    dplyr::relocate(c(z),
                    .after = t)
  
  # Create list of weights loadings
  mod_weights_list <- lapply(
    comp_vars,
    function(v) {
      
      wdf <- modelweights %>% 
        dplyr::filter(!stringr::str_detect(path,
                                           pattern = " \u00D7 "),
                      stringr::str_detect(path,
                                          pattern = paste0(v,"$"))) %>% 
        dplyr::select(path, boot_est) %>% 
        dplyr::rename(!!sym(v) := boot_est)
      
      return(wdf)
      
    }
  )
  
  # Reduce loadings into single data frame
  mod_weights <- suppressMessages(
    purrr::reduce(
      mod_weights_list,
      dplyr::full_join)) %>% 
    dplyr::mutate(variable = sapply(stringr::str_split(path,
                                                       "  ->  "),
                                    `[`,
                                    1)) %>% 
    dplyr::select(-path) %>% 
    dplyr::select(variable,
                  dplyr::everything())
  
  
  
  # -- TOTAL EFFECTS -- # 
  modeltoteffs <- as.data.frame(summarystat[["bootstrapped_total_paths"]]) %>%
    dplyr::rename(
      est = "Original Est.",
      boot_est = "Bootstrap Mean",
      boot_se = "Bootstrap SD",
      t = "T Stat.",
      lower_ci = "2.5% CI",
      upper_ci = "97.5% CI") %>% 
    tibble::rownames_to_column(var = "path") %>% 
    dplyr::mutate(path = gsub("\\*", 
                              " \u00D7 ", 
                              path),
                  z = boot_est / boot_se) %>% 
    dplyr::relocate(z, .after = t)
  
  
  
  # PATH ANALYSIS -----------------------------------------------------------

  # Set parameter for rounding
  rnd <- base::paste0('%.',
                      digits,
                      "f")
  
  # Get direct effect paths from z-statistic. Z-statistic generally preferred
  # due to bootstrap nature of PLS-SEM's p-value calculation. 
  # z-statistic calculated with (coef estimate) / (SE of coef)
  paths_tab <- base::as.data.frame(summarystat[["bootstrapped_paths"]]) %>% 
    
    # Get rownames as column
    tibble::rownames_to_column(var = "path") %>%
    
    # Remove t-statistic
    dplyr::select(-`T Stat.`) %>% 
    
    # Rename
    dplyr::rename(
      est = `Original Est.`,
      boot_est = `Bootstrap Mean`,
      boot_se = `Bootstrap SD`,
      lower_ci = `2.5% CI`,
      upper_ci = `97.5% CI`) %>% 
      
    # Create new variables
    dplyr::mutate(
      path = gsub("\\*", " \u00D7 ", path),
      z = boot_est / boot_se,
      p = 2 * pnorm(-abs(z))
      # in_text = paste("(\u03B2 = ",
      #                 sprintf(rnd,
      #                         boot_est),
      #                 ", SE = ", base::sprintf(rnd, boot_se),
      #                 ", z = ", sprintf(rnd,z),
      #                 ", 95% CI [", sprintf(rnd,lower_ci),
      #                 ", ", sprintf(rnd,upper_ci),
      #                 "], p ", ifelse(p < 0.001, "< 0.001", paste("=", sprintf(rnd,p))),")",
      #                 sep = ""),
      # fig_text = paste(sprintf(rnd, boot_est),
      #                  case_when(
      #                    
      #                    p > .05 ~ "",
      #                    p < .05 & p > .01 ~ "*",
      #                    p < .01 & p > .001 ~ "**",
      #                    p < .001 ~ "***"
      #                    
      #                  ),
      #                  " [", sprintf(rnd,lower_ci),
      #                  ", ", sprintf(rnd,upper_ci),
      #                  "]",
      #                  sep = "")
      ) %>% 
      
    # Relocate Z
    dplyr::relocate(c(z),
                    .after = boot_se)
  

  # RETURN RESULTS ----------------------------------------------------------

  # Pull into reportable sheets
  pls_sheets <- list(
    
    "PLS_Model" = pls_model,
    "PLS_Summary" = model_summary,
    "PLS_Composites" = model_cscores,
    "Boot_Model" = boot_mod,
    "Boot_Summary" = summarystat,
    "ModelR" = modelsum,
    "ModelES" = modeleffsize,
    "Reliability" = modelrelval,
    "Validity_FL" = fl_val,
    "Validity_HTMT" = htmt_val,
    "HTMT" = modelhtmt,
    "VIF" = modelvif,
    "Direct_Effects" = paths_tab,
    "Loadings_Table" = mod_loadings,
    "Loadings" = modelloadings,
    "Weights_Table" = mod_weights,
    "Weights" = modelweights,
    "Total_Effects" = modeltoteffs
    
  )
  

  # ADDITIONAL ANALYSES -----------------------------------------------------

  
  # -- INDIRECT EFFECTS -- #
  
  # If mediators exist, pull indirect effects table.
  if(!is.null(mediators)){
    
    # Define a function to generate the key and call specific_effect_significance
    call_specific_effect <- function(pvar, 
                                     mvar, 
                                     ovar, 
                                     svar = NULL, 
                                     alpha = 0.05) {
      
      # Define mediating pathway
      through <- if (is.null(svar)) mvar else c(mvar, svar)
      
      # Set indirect path key
      key <- paste(pvar, 
                   paste(through,
                         collapse = " -> "), 
                   ovar, 
                   sep = " -> ")
      
      # Get indirect effect
      value <- seminr::specific_effect_significance(
        
        boot_seminr_model = boot_mod, 
        from = pvar, 
        through = through, 
        to = ovar,
        alpha = alpha
        
      )
      
      list(key = key, 
           value = value)
      
    }
    
    # Generate all combinations and call the function
    if (is.null(serial_mediators)) {
      combinations <- expand.grid(predictors, mediators, outcomes)
      indtables <- apply(combinations,
                         1,
                         function(x) call_specific_effect(x[1], x[2], x[3]))
    } else {
      combinations1 <- expand.grid(predictors, mediators, outcomes)
      indtables1 <- apply(combinations1, 
                          1, 
                          function(x) call_specific_effect(x[1], x[2], x[3]))
      
      combinations2 <- expand.grid(predictors, mediators, serial_mediators, outcomes)
      indtables2 <- apply(combinations2, 
                          1, 
                          function(x) call_specific_effect(x[1], x[2], x[4], x[3]))
      
      combinations3 <- expand.grid(predictors, mediators, serial_mediators)
      indtables3 <- apply(combinations3, 
                          1, 
                          function(x) call_specific_effect(x[1], x[2], x[3]))
      
      combinations4 <- expand.grid(predictors, serial_mediators, outcomes)
      indtables4 <- apply(combinations4, 
                          1, 
                          function(x) call_specific_effect(x[1], x[2], x[3]))
      
      combinations5 <- expand.grid(mediators, serial_mediators, outcomes)
      indtables5 <- apply(combinations5, 
                          1, 
                          function(x) call_specific_effect(x[1], x[2], x[3]))
      
      indtables <- c(indtables1, 
                     indtables2, 
                     indtables3, 
                     indtables4, 
                     indtables5)
    }
    
    # Convert the list to a named list
    indtables <- stats::setNames(
      
      base::lapply(indtables,
                   `[[`,
                   "value"), 
      base::sapply(indtables, 
                   `[[`, 
                   "key")
      
    )
    
    # Compile into working table
    indirect_effects <- base::as.data.frame(
      base::do.call(rbind, indtables)
    ) %>% 
      dplyr::filter(!(`T Stat.` == "NaN")) %>% 
      tibble::rownames_to_column(var = "path") %>% 
      dplyr::select(-`T Stat.`) %>% 
      dplyr::rename(
        
        est = `Original Est.`,
        boot_est = `Bootstrap Mean`,
        boot_se = `Bootstrap SD`,
        lower_ci = `2.5% CI`,
        upper_ci = `97.5% CI`
        
      ) %>% 
      dplyr::mutate(path = base::gsub("\\*", " \u00D7 ", path),
                    z = boot_est / boot_se,
                    p = 2*stats::pnorm(-base::abs(z))
                    ) %>% 
    dplyr::relocate(z,
                    .after = boot_se)
    
    # Save to results
    pls_sheets[["Indirect_Effects"]] <- indirect_effects
    
  }
  
  
  
  # -- PLS-SEM PRIORITIZATION -- #
  
  # Run if optimizer is set to TRUE
  if (base::isTRUE(prioritization)) {
    
    # -- LOADINGS -- #
    if (prioritization_type == "loading") {
      
      # Calculate loadings optimizer
      prio <- calc_plssem_prioritization(
        
        loadings = modelloadings,
        data = data,
        total_estimates = modeltoteffs,
        outcomes = outcomes,
        weight_type = prioritization_weight_type,
        cor_method = prioritization_cor_method
        
      )
      
    }
    
    # -- WEIGHTS -- #
    if (prioritization_type == "weight") {
      
      # Calculate weights optimizer
      prio <- calc_plssem_prioritization(
        
        weights = modelweights,
        data = data,
        total_estimates = modeltoteffs,
        outcomes = outcomes,
        weight_type = prioritization_weight_type,
        cor_method = prioritization_cor_method
        
      )
      
    }
    
    # -- COMBINATION -- #
    if (prioritization_type == "combo") {
      
      # Calculate combinations optimizer
      prio <- calc_plssem_prioritization(
        
        loadings = modelloadings,
        weights = modelweights,
        data = data,
        total_estimates = modeltoteffs,
        outcomes = outcomes,
        weight_type = prioritization_weight_type,
        cor_method = prioritization_cor_method
        
      )
      
    }
    
    # Save to results
    pls_sheets[["Prioritization"]] <- prio
    
  }
  
  
  # GET META DATA -----------------------------------------------------------
  
  meta_data <- list(
    bootn = bootn,
    stop_criterion = stop_criterion,
    max_iter = max_iter,
    predictors = predictors,
    outcomes = outcomes,
    mediators = if(is.null(mediators)) {NULL} else {mediators},
    serial_mediators = if(is.null(serial_mediators)) {NULL} else {serial_mediators},
    paths = paths,
    indirect_effects = if(!is.null(mediators)) {TRUE} else {FALSE},
    prioritization = prioritization,
    prioritization_type = prioritization_type,
    prioritization_weight_type = prioritization_weight_type,
    prioritization_cor_method = prioritization_cor_method
  )
  
  # Save to results
  pls_sheets[["meta_data"]] <- meta_data

  
  # WRITE WORKBOOK ----------------------------------------------------------

  if (!is.null(filename)) {
    
    write_plssem_workbook(
      plssem_object = pls_sheets,
      filename = filename,
      study_name = study_name,
      digits = digits,
      path_tables = path_tables
    )
    
  }
  
  
  # Return
  return(pls_sheets)
  
}

#' @rdname wrap_plssem
#' @export
plssem <- wrap_plssem