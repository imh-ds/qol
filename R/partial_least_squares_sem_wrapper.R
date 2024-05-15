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
plssem_wrapper <- function(data = .,
                           measurements,
                           structure,
                           file = NULL,
                           bootn = 1000){
  
  # ---------------------------- #
  # -- EXTRACT STRUCTURE VARS -- #
  # ---------------------------- #
  
  # Create separate dataframe for structure to extract variables
  structure_df = structure %>% 
    as.data.frame()
  
  # Get unique values of all composites and latent vars in structure model
  vars = unique(c(unique(structure_df$source),
                  unique(structure_df$target)))
  
  # Identify endogenous mediators, serial mediators, and outcome vars
  paths = unique(structure_df$target)
  
  # Create empty lists for relevant model variables
  predictors_list = list()
  mediators_list = list()
  serial_mediators_list = list()
  outcomes_list = list()
  
  # Extract predictors & outcomes
  for(var in vars){
    
    # If var only shows up in "source" and not "target", assign as predictor
    if(var %in% structure_df$source && !var %in% structure_df$target){
      
      predictors_list[[var]] = paste(var)
      
    }
    
    # If var only shows up in "target" and not "source", assign as outcome
    if(!var %in% structure_df$source && var %in% structure_df$target){
      
      outcomes_list[[var]] = paste(var)
      
    }
    
  }
  
  # Reduce into single combined string
  predictors_bind = purrr::reduce(predictors_list, cbind)
  outcomes_bind = purrr::reduce(outcomes_list, cbind)
  
  predictors = c(predictors_bind)
  outcomes = c(outcomes_bind)
  
  # Exclude outcomes and get unique targets for mediator variables
  structure_df2 <- structure_df %>% 
    filter(!target %in% outcomes)
  
  med_vars <- unique(structure_df2$target)
  
  # Identify any pathways indicating serial mediators based on whether mediators are predicting mediators
  structure_df2 <- structure_df2 %>% 
    mutate(serial_flag = source %in% med_vars & target %in% med_vars)
  
  # Flag if there are any serial mediators; 1 if flagged, 0 if not.
  serial_flag <- as.integer(any(structure_df2$serial_flag))
  
  
  # Run if presence of serial mediators is identified
  if(serial_flag == 1){
    
    # Get second structural dataframe but exclude all predictors and outcomes
    structure_df2 <- structure_df %>% 
      filter(!source %in% predictors) %>% 
      filter(!target %in% outcomes)
    
    # Get vector of unique mediating variables
    med_vars = unique(c(unique(structure_df2$source),
                        unique(structure_df2$target)))
    
    # Extract mediating and serial mediating variables
    for(var in med_vars){
      
      # If variable is only in source and not target, assign as mediator
      if(var %in% structure_df2$source && !var %in% structure_df2$target){
        
        mediators_list[[var]] = paste(var)
        
      }
      
      # If variable is only in target and not in source, assign as serial mediator
      if(!var %in% structure_df2$source && var %in% structure_df2$target){
        
        serial_mediators_list[[var]] = paste(var)
        
      }
      
    }
    
    # If there are mediators, get vector, otherwise assign as NULL
    if(length(mediators_list) > 0){
      
      mediators_bind = purrr::reduce(mediators_list, cbind)
      mediators = c(mediators_bind)
      
    } else {
      
      mediators = NULL
      
    }
    
    # If there are serial mediators, get vector, otherwise assign as NULL
    if(length(serial_mediators_list) > 0){
      
      serial_mediators_bind = purrr::reduce(serial_mediators_list, cbind)
      serial_mediators = c(serial_mediators_bind)
      
    } else {
      
      serial_mediators = NULL
      
    }
    
  } else {
    
    mediators = med_vars
    serial_mediators = NULL
    
  }
  
  # If mediators is length 0, then assign NULL
  if(length(mediators) == 0){
    
    mediators = NULL
    
  }
  
  
  # ---------------------------- #
  # -- PLS-SEM BASIC ANALYSIS -- #
  # ---------------------------- #
  
  # Run PLS-SEM analysis
  pls_model = estimate_pls(data = data,
                           measurement_model = measurements,
                           structural_model = structure)
  
  # Get PLS-SEM summary of results
  model_summary = summary(pls_model)
  
  # Get general model path summary
  modelsum = model_summary$paths %>%
    as.data.frame() %>%
    rownames_to_column(var = "Variable")
  
  # Get model effect sizes
  modeleffsize = model_summary$fSquare %>%
    as.data.frame() %>%
    rownames_to_column(var = "Variable")
  
  # Get model reliability statistics
  modelreliability = model_summary$reliability %>%
    as.data.frame() %>%
    rownames_to_column(var = "Variable")
  
  # Create empty list to assign VIF statistics
  viftables = list()
  
  # Loop and assign VIF statistics 
  for (pathvar in paths){
    viftables[[pathvar]] = model_summary[["vif_antecedents"]][[pathvar]] %>%
      as.data.frame() %>%
      rownames_to_column(var = "Variable") %>%
      rename(!!paste0(pathvar) := ".")
  }
  
  # Collapse VIF stats together into table
  modelvif = viftables %>%
    purrr::reduce(full_join)
  
  
  
  
  
  # --------------------------- #
  # -- PLS-SEM BOOTSTRAPPING -- #
  # --------------------------- #
  
  # Boostrap PLS-SEM to get confidence intervals
  boot_mod1 = bootstrap_model(seminr_model = pls_model,
                              nboot = bootn)
  
  # Get summary statistics
  summarystat = summary(boot_mod1) #Create object
  
  # Create degrees of freedom numbers based on outcome variable
  n = nrow(pls_model$construct_scores)
  
  # Degrees of freedom for outcome path
  df_out = n - (1 + length(predictors) + length(serial_mediators) + length(mediators))
  # Degrees of freedom for serial mediator path
  df_smd = n - (1 + length(predictors) + length(mediators))
  # Degrees of freedom for mediator path
  df_med = n - (1 + length(predictors))
  
  # Get direct effect paths from t-statistic
  paths_t = as.data.frame(summarystat$bootstrapped_paths) %>% 
    rownames_to_column(var = "Path") %>%
    rename(`Bootstrap SE` = `Bootstrap SD`) %>%
    mutate(p.value = 2*pt(q = abs(`T Stat.`),
                          df = ifelse(str_extract(Path, "(?<=  ->  )[^->]+$") %in% outcomes,
                                      df_out,
                                      ifelse(str_extract(Path, "(?<=  ->  )[^->]+$") %in% serial_mediators,
                                             df_smd,
                                             df_med)),
                          lower.tail = F),
           text = paste("(beta = ",sprintf('%.3f',`Bootstrap Mean`),
                        ", t(",sprintf('%.f',ifelse(str_extract(Path, "(?<=  ->  )[^->]+$") %in% outcomes,
                                                    df_out,
                                                    ifelse(str_extract(Path, "(?<=  ->  )[^->]+$") %in% serial_mediators,
                                                           df_smd,
                                                           df_med))),
                        ") = ",sprintf('%.3f',`T Stat.`),
                        ", 95% CI [",sprintf('%.3f',`2.5% CI`),", ",sprintf('%.3f',`97.5% CI`),
                        "], p ", ifelse(p.value < 0.001, "< 0.001", paste("=",sprintf('%.3f',p.value))),")",
                        sep = ""),
           fig.text = paste(sprintf('%.3f',`Bootstrap Mean`),
                            ifelse(p.value > .05, "",
                                   ifelse(p.value < .05 & p.value > .01, "*",
                                          ifelse(p.value < .01 & p.value > .001, "**", "***"))),
                            " [", sprintf('%.3f',`2.5% CI`),
                            ", ", sprintf('%.3f',`97.5% CI`),
                            "]",
                            sep = ""))
  
  # Get direct effect paths from z-statistic
  # z-statistic generally preferred due to bootstrap nature of PLS-SEM's p-value calculation
  paths_z = as.data.frame(summarystat$bootstrapped_paths) %>% 
    rownames_to_column(var = "Path") %>%
    rename(`Bootstrap SE` = `Bootstrap SD`) %>% # Renamed as `seminr` labels standard error as standard deviation
    mutate(z = `Bootstrap Mean` / `Bootstrap SE`, # z-statistic calculated with (coef estimate) / (SE of coef)
           p.value = 2*pnorm(-abs(z)),
           text = paste("(beta = ",sprintf('%.3f',`Bootstrap Mean`),
                        ", z(",sprintf('%.f',ifelse(str_extract(Path, "(?<=  ->  )[^->]+$") %in% outcomes,
                                                    df_out,
                                                    ifelse(str_extract(Path, "(?<=  ->  )[^->]+$") %in% serial_mediators,
                                                           df_smd,
                                                           df_med))),
                        ") = ",sprintf('%.3f',z),
                        ", 95% CI [",sprintf('%.3f',`2.5% CI`),", ",sprintf('%.3f',`97.5% CI`),
                        "], p ", ifelse(p.value < 0.001, "< 0.001", paste("=",sprintf('%.3f',p.value))),")",
                        sep = ""),
           fig.text = paste(sprintf('%.3f',`Bootstrap Mean`),
                            ifelse(p.value > .05, "",
                                   ifelse(p.value < .05 & p.value > .01, "*",
                                          ifelse(p.value < .01 & p.value > .001, "**", "***"))),
                            " [", sprintf('%.3f',`2.5% CI`),
                            ", ", sprintf('%.3f',`97.5% CI`),
                            "]",
                            sep = ""))
  
  # Create empty lists for regression tables
  path_table_v1 = list()
  path_table_v2 = list()
  
  
  # -------------------------------------- #
  # -- REGRESSION TABLES IN LONG FORMAT -- #
  # -------------------------------------- #
  
  
  # Loop for every endogenous variables (e.g., mediators,)
  for (var in paths){
    
    pt_outcome <- paths_z %>%
      as.data.frame() %>% 
      filter(grepl(paste0(var,"$"),Path))
    
    pt_est = pt_outcome %>% 
      mutate(std_est = paste(sprintf('%.3f', `Bootstrap Mean`),
                             ifelse(p.value > .05, "",
                                    ifelse(p.value < .05 & p.value > .01, "*",
                                           ifelse(p.value < .01 & p.value > .001, "**", "***"))), sep = " ")) %>% 
      dplyr::select(Path, std_est)
    
    pt_ci <- pt_outcome %>% 
      mutate(ci = paste0("(",
                         sprintf('%.3f',`2.5% CI`),
                         ", ",
                         sprintf('%.3f',`97.5% CI`),
                         ")")) %>% 
      dplyr::select(Path, ci)
    
    # Get degrees of freedom for number of predictors (numerator)
    degfree1 <- length(pt_est$Path)
    # Get degrees of freedom for number of observations minus predictors - 1 (denominator)
    degfree2 <- nrow(pls_model$construct_scores) - (1 + degfree1)
    
    pt_df1 <- data.frame("EST" = c(rbind(pt_est$std_est, pt_ci$ci)))
    pt_df2 <- data.frame("Variable" = c(rbind(pt_est$Path,
                                              paste0(sub("(.*)  ->  .*", "\\1", pt_est$Path),"_CI")))) %>% 
      mutate(Variable = gsub("  ->.*", "",Variable))
    pt_df3 <- data.frame("Variable" = c("Observations",
                                        "R^2",
                                        "R^2 Adj"),
                         "EST" = c(nrow(pls_model$construct_scores),
                                   sprintf('%.3f',as.data.frame(pls_model$rSquared)[[var]])))
    
    # Get R^2 value for F-statistic calculation
    rsqr <- as.data.frame(pls_model$rSquared)[[var]][1]
    
    # Calculate F-statistic
    pt_df4 <- data.frame("Variable" = c("F"),
                         "EST" = sprintf('%.3f', (rsqr / (1 - rsqr)) * (degfree2 / degfree1)))
    
    pt_table = pt_df2 %>%
      cbind(pt_df1) %>% 
      rbind(pt_df3) %>% 
      rbind(pt_df4) %>% 
      rename(!!sym(paste0(var,
                          " Beta (95% CI)")) := EST)
    
    path_table_v1[[paste0(var,"_V1")]] = pt_table
    
  }
  
  # Collapse the path table list into one dataframe and send R^2 to the bottom
  pt_tab_eff <- path_table_v1 %>%
    purrr::reduce(full_join) %>%
    as.data.frame() %>% 
    filter(!grepl("R\\^2|^Observations$|^F$",
                  Variable))
  
  pt_tab_desc <- path_table_v1 %>%
    purrr::reduce(full_join) %>%
    as.data.frame() %>% 
    filter(grepl("R\\^2|^Observations$|^F$",
                 Variable))
  
  # Combine into final product
  pathtab1 <- rbind(pt_tab_eff,
                    pt_tab_desc) %>% 
    mutate(Variable = ifelse(grepl("_CI", Variable), "", Variable)) %>% 
    magrittr::set_colnames(.,
                           c("Variable",
                             rep("Beta (95% CI)", length(paths))))
  
  # Get Model Numbers
  pathtab1 <- rbind(names(pathtab1),
                    pathtab1) %>% 
    magrittr::set_colnames(.,
                           c("",
                             paste0(sprintf("Model %01d ", seq(length(paths))),
                                    paths)))
  
  
  
  # -------------------------------------- #
  # -- REGRESSION TABLES IN WIDE FORMAT -- #
  # -------------------------------------- #
  
  for (var in paths){
    
    # Get direct effects relevant to exogenous outcome
    pt_outcome = paths_z %>%
      as.data.frame() %>% 
      filter(grepl(paste0(var,"$"),Path))
    
    # Select only relevant metrics (beta, standard error, p-value)
    pt_est = pt_outcome %>% 
      mutate(std_est = paste(sprintf('%.3f', `Bootstrap Mean`),
                             ifelse(p.value > .05, "",
                                    ifelse(p.value < .05 & p.value > .01, "*",
                                           ifelse(p.value < .01 & p.value > .001, "**", "***"))), sep = " "),
             std_se = sprintf("%.3f",`Bootstrap SE`),
             p_val = ifelse(p.value < .001, "<.001", sprintf("%.3f", p.value))) %>% 
      dplyr::select(Path, std_est, std_se, p_val)
    
    # Grab 95% confidence intervals
    pt_ci = pt_outcome %>% 
      mutate(ci = paste0("(",
                         sprintf('%.3f',`2.5% CI`),
                         ", ",
                         sprintf('%.3f',`97.5% CI`),
                         ")")) %>% 
      dplyr::select(Path, ci)
    
    # Clean up labels of variables
    pt_df1 = pt_est %>% 
      inner_join(pt_ci) %>% 
      dplyr::select(Path, std_est, std_se, ci, p_val) %>% 
      mutate(Path = gsub("  ->.*", "",Path))
    
    # Get basic model descriptives and metrics
    pt_df2 = data.frame("Path" = c("Observations",
                                   "R^2",
                                   "R^2 Adj"),
                        "std_est" = c(nrow(pls_model$construct_scores),
                                      sprintf('%.3f',as.data.frame(pls_model$rSquared)[[var]])),
                        "std_se" = rep("",3),
                        "ci" = rep("",3),
                        "p_val" = rep("",3))
    
    # Get degrees of freedom for number of predictors (numerator)
    degfree1 <- length(pt_est$Path)
    # Get degrees of freedom for number of observations minus predictors - 1 (denominator)
    degfree2 <- nrow(pls_model$construct_scores) - (1 + degfree1)
    
    # Get R^2 value for F-statistic calculation
    rsqr <- as.data.frame(pls_model$rSquared)[[var]][1]
    
    # Calculate F-statistic
    pt_df4 <- data.frame("Variable" = c("F"),
                         "EST" = sprintf('%.3f', (rsqr / (1 - rsqr)) * (degfree2 / degfree1)))
    
    pt_df3 <- data.frame("Path" = c("F"),
                         "std_est" = sprintf('%.3f', (rsqr / (1 - rsqr)) * (degfree2 / degfree1)),
                         "std_se" = "",
                         "ci" = "",
                         "p_val" = "")
    
    pt_table = pt_df1 %>%
      rbind(pt_df2,
            pt_df3) %>% 
      rename("Variable" = Path,
             !!sym(paste0(var," std_est")) := std_est,
             !!sym(paste0(var," std_se")) := std_se,
             !!sym(paste0(var," ci")) := ci,
             !!sym(paste0(var," p_val")) := p_val)
    
    path_table_v2[[paste0(var,"_V2")]] = pt_table
    
  }
  
  # Collapse the path table list into one dataframe and send R^2 to the bottom
  pt_wtab_eff <- path_table_v2 %>%
    purrr::reduce(full_join) %>%
    as.data.frame() %>% 
    filter(!grepl("R\\^2|^Observations$|^F$",
                  Variable))
  
  pt_wtab_desc <- path_table_v2 %>%
    purrr::reduce(full_join) %>%
    as.data.frame() %>% 
    filter(grepl("R\\^2|^Observations$|^F$",
                 Variable))
  
  # Combine into final product
  pathtab2 <- rbind(pt_wtab_eff,
                    pt_wtab_desc) %>% 
    magrittr::set_colnames(.,
                           c("Variable",
                             rep(c("Beta",
                                   "SE",
                                   "95% CI (Lower, Upper)",
                                   "p"),
                                 length(paths))))
  
  # Get model numbers and exogenous variable
  pt2_names <- c("",
                 c(rbind(paste0(sprintf("Model %01d",
                                        seq(length(paths))),
                                " ",
                                paths), "", "", "")))
  
  # Rename to combine with model numbers and exogenous variable
  pathtab2 <- rbind(names(pathtab2),
                    pathtab2) %>% 
    magrittr::set_colnames(pt2_names)
  
  
  
  # -------------------------------- #
  # -- ADDITIONAL PLS-SEM METRICS -- #
  # -------------------------------- #
  
  #Create loadings dataframe
  loadings = as.data.frame(summarystat$bootstrapped_loadings) %>%
    rownames_to_column(var = "Path") %>%
    rename(`Bootstrap SE` = `Bootstrap SD`) %>%
    mutate(z = `Bootstrap Mean` / `Bootstrap SE`,
           p.value = 2*pnorm(-abs(z)))
  
  #Create weights dataframe
  weights = as.data.frame(summarystat$bootstrapped_weights) %>%
    rownames_to_column(var = "Path")
  
  #Create total paths dataframe
  totpaths = as.data.frame(summarystat$bootstrapped_total_paths) %>%
    rownames_to_column(var = "Path")
  
  #Create HTMT dataframe
  htmt = as.data.frame(summarystat$bootstrapped_HTMT) %>%
    rownames_to_column(var = "Path")
  
  # Running all possible combinations of predictors -> mediators -> outcomes
  if(is.null(serial_mediators) == TRUE){
    
    indtables = list()
    
    for(pvar in predictors){
      for(mvar in mediators){
        for(ovar in outcomes){
          indtables[[paste(pvar,mvar,ovar,sep = " -> ")]] = specific_effect_significance(boot_seminr_model = boot_mod1,
                                                                                         from = pvar,
                                                                                         through = mvar,
                                                                                         to = ovar,
                                                                                         alpha = 0.05)
        }
      }
    }
  } else {
    
    # Flagged if serial_mediators indicates that there are second serial mediators
    # Running all possible combinations of predictors -> mediators -> second mediators -> outcomes
    
    indtables1 = list()
    
    for(pvar in predictors){
      for(mvar in mediators){
        for(ovar in outcomes){
          indtables1[[paste(pvar,mvar,ovar, sep = " -> ")]] = specific_effect_significance(boot_seminr_model = boot_mod1,
                                                                                           from = pvar,
                                                                                           through = mvar,
                                                                                           to = ovar,
                                                                                           alpha = 0.05)
        }
      }
    }
    
    indtables2 = list()
    
    for(pvar in predictors){
      for(mvar in mediators){
        for(svar in serial_mediators){
          for(ovar in outcomes){
            indtables2[[paste(pvar,mvar,svar,ovar, sep = " -> ")]] = specific_effect_significance(boot_seminr_model = boot_mod1,
                                                                                                  from = pvar,
                                                                                                  through = c(mvar,svar),
                                                                                                  to = ovar,
                                                                                                  alpha = 0.05)
          } 
        }
      }
    }
    
    indtables3 = list()
    
    for(pvar in predictors){
      for(mvar in mediators){
        for(svar in serial_mediators){
          indtables3[[paste(pvar,mvar,svar, sep = " -> ")]] = specific_effect_significance(boot_seminr_model = boot_mod1,
                                                                                           from = pvar,
                                                                                           through = mvar,
                                                                                           to = svar,
                                                                                           alpha = 0.05)
        }
      }
    }
    
    indtables4 = list()
    
    for(pvar in predictors){
      for(svar in serial_mediators){
        for(ovar in outcomes){
          indtables4[[paste(pvar,svar,ovar, sep = " -> ")]] = specific_effect_significance(boot_seminr_model = boot_mod1,
                                                                                           from = pvar,
                                                                                           through = svar,
                                                                                           to = ovar,
                                                                                           alpha = 0.05)
        }
      }
    }
    
    indtables5 = list()
    
    for(mvar in mediators){
      for(svar in serial_mediators){
        for(ovar in outcomes){
          indtables5[[paste(mvar,svar,ovar, sep = " -> ")]] = specific_effect_significance(boot_seminr_model = boot_mod1,
                                                                                           from = mvar,
                                                                                           through = svar,
                                                                                           to = ovar,
                                                                                           alpha = 0.05)
        }
      }
    }
    
    indtables_full = list(indtables1, indtables2, indtables3, indtables4, indtables5)
    indtables = lapply(rapply(indtables_full, enquote, how="unlist"), eval)
    
  }
  
  # Create a table of indirect effects
  if(!is.null(mediators)){
    
    indirect_effects = indtables %>%
      do.call(rbind, .) %>% 
      as.data.frame() %>% 
      rownames_to_column(var = "Path") %>%
      mutate(Path = names(indtables)) %>% 
      rename(`Bootstrap SE` = `Bootstrap SD`) %>%
      mutate(z = `Bootstrap Mean` / `Bootstrap SE`,
             p.value = 2*pnorm(-abs(z)),
             text = paste0("(beta = ", sprintf('%.3f', `Bootstrap Mean`),
                           ", SE = ", sprintf('%.3f', `Bootstrap SE`),
                           ", z = ", sprintf('%.3f', z),
                           ", 95% CI [", sprintf('%.3f', `2.5% CI`),
                           ", ", sprintf('%.3f', `97.5% CI`),
                           "], p ", ifelse(p.value < 0.001, "< 0.001",
                                           paste("=", sprintf('%.3f', p.value))),")"))
    
  }
  
  if(is.null(mediators) & is.null(serial_mediators)){
    
    sheets = list("PLS_Model" = pls_model,
                  "PLS_Summary" = model_summary,
                  "Boot_Model" = boot_mod1,
                  "Boot_Summary" = summarystat,
                  "Export" = list("ModelR" = modelsum,
                                  "ModelES" = modeleffsize,
                                  "Reliability" = modelreliability,
                                  "VIF" = modelvif,
                                  "Paths_z" = paths_z,
                                  "Paths_t" = paths_t,
                                  "Tables_Long" = pathtab1,
                                  "Tables_Wide" = pathtab2,
                                  "Loadings" = loadings,
                                  "Weights" = weights,
                                  "TotPaths" = totpaths,
                                  "HTMT" = htmt))
    
  } else {
    
    sheets = list("PLS_Model" = pls_model,
                  "PLS_Summary" = model_summary,
                  "Boot_Model" = boot_mod1,
                  "Boot_Summary" = summarystat,
                  "Export" = list("ModelR" = modelsum,
                                  "ModelES" = modeleffsize,
                                  "Reliability" = modelreliability,
                                  "VIF" = modelvif,
                                  "Paths_z" = paths_z,
                                  "Paths_t" = paths_t,
                                  "Tables_Long" = pathtab1,
                                  "Tables_Wide" = pathtab2,
                                  "Loadings" = loadings,
                                  "Weights" = weights,
                                  "TotPaths" = totpaths,
                                  "HTMT" = htmt,
                                  "Indirect" = indirect_effects))
    
  }
  
  
  if(is.null(file) == FALSE){
    
    writexl::write_xlsx(sheets$Export, path = file)
    
  }
  
  return(sheets)
  
}