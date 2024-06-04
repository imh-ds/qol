#' Partial Least Squares Structural Equation Modeling Wrapper
#' 
#' @description
#' A wrapper function to automate PLS-SEM and extracting its results. The
#' current function is limited to only 2 serial mediators. Due to the undue
#' level of complexity added to the model estimations when there are 3 or
#' more serial mediators, the current function is not designed to handle this
#' type of model. This automation function uses the \code{seminr} package (Ray et
#' al., 2022).
#' 
#' @param data A dataframe object. This should be a structured dataset where
#' each column represents a variable and each row represents an observation.
#' @param measurements Measurement model object from \code{seminr} package. Refer
#' to the \code{seminr} help documentation for more details.
#' @param structure Structural model object from \code{seminr} package.
#' @param file Location to save output file as excel workbook if specified.
#' @param bootn Number of bootstrap replications to calculate p-values at the
#' structural pathways. Default to 1000.
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
#' Estimating Structural Equation Models. R package version 2.3.2,
#' \url{https://CRAN.R-project.org/package=seminr}.
#' 
#' @export
plssem_wrapper <- function(
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
    optimizer = FALSE,
    optimizer_type = "combo",
    optimizer_weight_type = "combo",
    optimizer_cor_method = "pearson") {
  

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
    qol::replace_with_na(variables = dplyr::everything(),
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
                  dplyr::everything()) %>% 
    
    # Normalize
    dplyr::mutate(dplyr::across(
      .cols = -1,
      \(x) x / sum(x,
                   na.rm = T)
    ))
  
  
  
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
      p = 2 * pnorm(-abs(z)),
      in_text = paste("(\u03B2 = ",
                      sprintf(rnd,
                              boot_est),
                      ", z = ", sprintf(rnd,z),
                      ", 95% CI [", sprintf(rnd,lower_ci),
                      ", ", sprintf(rnd,upper_ci),
                      "], p ", ifelse(p < 0.001, "< 0.001", paste("=", sprintf(rnd,p))),")",
                      sep = ""),
      fig_text = paste(sprintf(rnd, boot_est),
                       case_when(
                         
                         p > .05 ~ "",
                         p < .05 & p > .01 ~ "*",
                         p < .01 & p > .001 ~ "**",
                         p < .001 ~ "***"
                         
                       ),
                       " [", sprintf(rnd,lower_ci),
                       ", ", sprintf(rnd,upper_ci),
                       "]",
                       sep = "")) %>% 
      
    # Relocate Z
    dplyr::relocate(c(z),
                    .after = boot_se)
  
  

  # REGRESSION TABLE - LONG FORMAT ------------------------------------------

  # Loop for every endogenous variables (e.g., mediators,)
  reg_list_long <- base::lapply(
    
    paths,
    function(var){
      
      pt_outcome <- paths_tab %>%
        dplyr::filter(grepl(paste0(var,"$"),
                            path))
      
      pt_est <- pt_outcome %>% 
        dplyr::mutate(std_est = paste(sprintf(rnd, boot_est),
                                      case_when(
                                        
                                        p > .05 ~ "",
                                        p < .05 & p > .01 ~ "*",
                                        p < .01 & p > .001 ~ "**",
                                        p < .001 ~ "***"
                                        
                                      ))) %>% 
        dplyr::select(path, std_est)
      
      pt_ci <- pt_outcome %>% 
        dplyr::mutate(ci = paste0("(",
                                  sprintf(rnd, lower_ci),
                                  ", ",
                                  sprintf(rnd, upper_ci),
                                  ")")) %>% 
        dplyr::select(path,
                      ci)
      
      # Get degrees of freedom for number of predictors (numerator)
      degfree1 <- length(pt_est[["path"]])
      
      # Get degrees of freedom for number of observations minus predictors - 1 (denominator)
      degfree2 <- nrow(pls_model[["construct_scores"]]) - (1 + degfree1)
      
      pt_df1 <- data.frame("est" = c(rbind(pt_est[["std_est"]],
                                           pt_ci[["ci"]])))
      pt_df2 <- data.frame("variable" = c(rbind(pt_est[["path"]],
                                                paste0(sub("(.*)  ->  .*", "\\1",
                                                           pt_est[["path"]]),"_CI")))) %>% 
        dplyr::mutate(variable = gsub("  ->.*", "", variable))
      pt_df3 <- data.frame("variable" = c("Observations",
                                          "R\u00B2",
                                          "R\u00B2 Adj"),
                           "est" = c(nrow(pls_model[["construct_scores"]]),
                                     sprintf(rnd,
                                             as.data.frame(pls_model[["rSquared"]])[[var]])))
      
      # Get R^2 value for F-statistic calculation
      rsqr <- as.data.frame(pls_model[["rSquared"]])[[var]][1]
      
      # Calculate F-statistic
      pt_df4 <- data.frame("variable" = c("F"),
                           "est" = sprintf(rnd, 
                                           (rsqr / (1 - rsqr)) * (degfree2 / degfree1)))
      
      pt_table <- pt_df2 %>%
        cbind(pt_df1) %>% 
        rbind(pt_df3) %>% 
        rbind(pt_df4) %>% 
        dplyr::rename(!!sym(paste0(var,
                                   " \u03B2 (95% CI)")) := est)
      
      return(pt_table)
      
    }
    
  )
  
  # Reduce
  reg_tab_long <- base::suppressMessages(
      
      purrr::reduce(reg_list_long,
                    dplyr::full_join)
      
    )
  
  # Collapse the path table list into one dataframe and send R^2 to the bottom
  reg_tab_longeff <- reg_tab_long %>%
    dplyr::filter(
      
      !base::grepl("R\\\u00B2|^Observations$|^F$",
                   variable)
      
    )
  
  reg_tab_longdesc <- reg_tab_long %>%
    dplyr::filter(
      
      base::grepl("R\\\u00B2|^Observations$|^F$",
                  variable)
      
    )
  
  # Combine into final product
  reg_long_table <- rbind(reg_tab_longeff,
                        reg_tab_longdesc) %>% 
    dplyr::mutate(variable = ifelse(grepl("_CI", variable), "", variable)) %>% 
    dplyr::mutate(variable = gsub("\\*", " \u00D7 ", variable)) %>% 
    magrittr::set_colnames(.,
                           c("Variable",
                             rep("\u03B2 (95% CI)",
                                 length(paths))))
  
  # Get Model Numbers
  reg_long_table <- rbind(names(reg_long_table),
                          reg_long_table) %>% 
    magrittr::set_colnames(.,
                           c("",
                             paste0(sprintf("Model %01d ", seq(length(paths))),
                                    paths)))
  

  # REGRESSION TABLE - WIDE FORMAT ------------------------------------------

  reg_list_wide <- lapply(
    paths,
    function(var) {
      
      pt_outcome <- paths_tab %>%
        dplyr::filter(grepl(paste0(var,"$"),
                            path))
      
      pt_est <- pt_outcome %>% 
        dplyr::select(path,
                      boot_est,
                      lower_ci,
                      upper_ci,
                      p)
      
      # Clean up labels of variables
      pt_reg <- pt_est %>% 
        dplyr::mutate(path = gsub("  ->.*", "", path))
      
      # Get basic model descriptives and metrics
      pt_desc <- data.frame("path" = c("Observations",
                                       "R\u00B2",
                                       "R\u00B2 Adj"),
                            "boot_est" = c(nrow(pls_model[["construct_scores"]]),
                                           as.data.frame(pls_model$rSquared)[[var]]),
                            "lower_ci" = rep("",3),
                            "upper_ci" = rep("",3),
                            "p" = rep("",3))
      
      # Get degrees of freedom for number of predictors (numerator)
      degfree1 <- length(pt_est[["path"]])
      # Get degrees of freedom for number of observations minus predictors - 1 (denominator)
      degfree2 <- nrow(pls_model[["construct_scores"]]) - (1 + degfree1)
      
      # Get R^2 value for F-statistic calculation
      rsqr <- as.data.frame(pls_model[["rSquared"]])[[var]][1]
      
      # Calculate F-statistic
      pt_fstat <- data.frame("path" = c("F"),
                             "boot_est" = (rsqr / (1 - rsqr)) * (degfree2 / degfree1),
                             "lower_ci" = "",
                             "upper_ci" = "",
                             "p" = "")
      
      # Create table
      pt_table <- rbind(pt_reg,
                        pt_desc,
                        pt_fstat) %>%
        dplyr::rename("variable" = path,
                      !!sym(paste0(var," est")) := boot_est,
                      !!sym(paste0(var," lower")) := lower_ci,
                      !!sym(paste0(var," upper")) := upper_ci,
                      !!sym(paste0(var," p")) := p)
      
      # Return
      return(pt_table)
      
    }
  )
  
  # Collapse the path table list into one dataframe and send R^2 to the bottom
  reg_tab_wideeff <- suppressMessages(purrr::reduce(reg_list_wide,
                                                    dplyr::full_join)) %>%
    dplyr::filter(!grepl("R\\\u00B2|^Observations$|^F$",
                         variable))
  
  reg_tab_widedesc <- suppressMessages(purrr::reduce(reg_list_wide,
                                                     dplyr::full_join)) %>%
    dplyr::filter(grepl("R\\\u00B2|^Observations$|^F$",
                        variable))
  
  # Combine into final product
  reg_wide_table <- rbind(reg_tab_wideeff,
                          reg_tab_widedesc) %>% 
    dplyr::mutate(variable = gsub("\\*", " \u00D7 ", variable)) %>% 
    magrittr::set_colnames(.,
                           c("Variable",
                             rep(c("\u03B2",
                                   "Lower",
                                   "Upper",
                                   "p"),
                                 length(paths))))
  
  # Get model numbers and exogenous variable
  reg_wide_table_names <- c("",
                            c(rbind(paste0(sprintf("Model %01d",
                                                   seq(length(paths))),
                                           " ",
                                           paths), 
                                    "", "", "")))
  
  # Rename to combine with model numbers and exogenous variable
  reg_wide_table <- rbind(names(reg_wide_table),
                          reg_wide_table) %>% 
    magrittr::set_colnames(reg_wide_table_names)

  

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
    "Pathway_Table" = paths_tab,
    "Loadings_Table" = mod_loadings,
    "Loadings" = modelloadings,
    "Weights_Table" = mod_weights,
    "Weights" = modelweights,
    "Total_Effects" = modeltoteffs,
    "Tables_Long" = reg_long_table,
    "Tables_Wide" = reg_wide_table
    
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
                    p = 2*stats::pnorm(-base::abs(z)),
                    in_text = base::paste0(
                      "(\u03B2 = ", base::sprintf(rnd, boot_est),
                      ", SE = ", base::sprintf(rnd, boot_se),
                      ", z = ", base::sprintf(rnd, z),
                      ", 95% CI [", base::sprintf(rnd, lower_ci),
                      ", ", base::sprintf(rnd, upper_ci),
                      "], p ", base::ifelse(p < 0.001, "< 0.001",
                                            base::paste("=", 
                                                        base::sprintf(rnd, 
                                                                      p))),
                      ")")) %>% 
    dplyr::relocate(z,
                    .after = boot_se)
    
    # Save to results
    pls_sheets[["Indirect_Effects"]] <- indirect_effects
    
  }
  
  
  
  # -- PLS-SEM OPTIMIZER -- #
  
  # Run if optimizer is set to TRUE
  if (base::isTRUE(optimizer)) {
    
    # -- LOADINGS -- #
    if (optimizer_type == "loading") {
      
      # Calculate loadings optimizer
      opt <- qol::plssem_optimizer(
        
        loadings = modelloadings,
        data = data,
        total_estimates = modeltoteffs,
        outcomes = outcomes,
        weight_type = optimizer_weight_type,
        cor_method = optimizer_cor_method
        
      )
      
    }
    
    # -- WEIGHTS -- #
    if (optimizer_type == "weight") {
      
      # Calculate weights optimizer
      opt <- qol::plssem_optimizer(
        
        weights = modelweights,
        data = data,
        total_estimates = modeltoteffs,
        outcomes = outcomes,
        weight_type = optimizer_weight_type,
        cor_method = optimizer_cor_method
        
      )
      
    }
    
    # -- COMBINATION -- #
    if (optimizer_type == "combo") {
      
      # Calculate combinations optimizer
      opt <- qol::plssem_optimizer(
        
        loadings = modelloadings,
        weights = modelweights,
        data = data,
        total_estimates = modeltoteffs,
        outcomes = outcomes,
        weight_type = optimizer_weight_type,
        cor_method = optimizer_cor_method
        
      )
      
    }
    
    # Save to results
    pls_sheets[["Optimizer"]] <- opt
    
  }
  
  # Return
  return(pls_sheets)
  
}
