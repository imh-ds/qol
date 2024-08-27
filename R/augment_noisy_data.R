#' Augment Noisy Data
#'
#' @description Augment a dataset by generating a noisy synthetic copy of the
#'   observed data using random forest.
#'
#' @details This function augments a dataset by introducing a specified
#'   percentage of missingness into each column, using random forest imputation
#'   to learn from the data, and generating synthetic data based on the trained
#'   models. The process involves the following steps:
#'
#'  \enumerate{
#'  \item Inject a specified percentage (XX\%) of missingness into each variable of
#'    interest (VOI) in the observed data. This step helps to reduce the risk
#'    of overfitting the Random Forest models to the measurement noise in
#'    the observed data.
#'  \item Use Random Forest imputation to train models on the data and store all
#'    trained models.
#'  \item Resample the observed data with or without replacement to the specified
#'    sample size (n). If n is greater than the original sample size,
#'    replacement is automatically used.
#'  \item Introduce random noise into the resampled data to mimic the natural
#'    measurement noise that arises from larger population samples.
#'  \item Use the trained Random Forest models from Step 2 to generate predicted
#'    values for each VOI using the noisy data from Step 4 as input. This
#'    creates a synthetic dataset.
#'  \item Inject XX\% of random missingness into the noisy data from Step 4. Replace
#'    the missing values with the predicted values from Step 5, combining the
#'    noisy data and synthetic data to create the final augmented dataset.
#' }
#'
#'   These steps are repeated for all possible pairwise combinations of XX\%
#'   missingness in the training dataset (Step 1) and XX\% of the noisy data to
#'   be replaced with predicted values (Step 6), resulting in multiple augmented
#'   datasets. This process can be iterated N times to account for the random
#'   nature of missingness injection, which may produce varying qualities of
#'   augmented data each time. Each augmented dataset is compared against the
#'   observed data using several metrics:
#'
#' \itemize{
#'  \item Mean (M) of each VOI
#'  \item Standard Deviation (SD) of each VOI
#'  \item Average inter-correlation of each VOI
#'  \item Skewness of each VOI
#'  \item Kurtosis of each VOI
#' }
#'
#'   Optionally, an 'error correction' penalization can be applied to models
#'   with stronger average inter-item correlations than the observed data to
#'   minimize the inflation of Type I error due to overfitting.
#'
#'   The average differences between augmented and observed data are
#'   SD-weighted, favoring models with fewer varied differences across the VOI.
#'   Skewness and kurtosis of VOI are assessed to determine whether they more
#'   closely match the observed data's distribution (bias preservation) or
#'   achieve a more normal distribution (bias correction).
#'
#'   The function identifies the model that yields the best parameters across
#'   all metrics (M, SD, Cor, Skew, Kurtosis) and returns the best augmented
#'   dataset along with its corresponding metadata.
#'
#' @param data A dataframe object. This should be a structured dataset where
#'   each column represents a variable and each row represents an observation.
#' @param vars An optional vector of variable names that should be augmented. If
#'   \code{vars} is not specified, the function will default to augmenting the
#'   entire dataset (not recommended).
#' @param covariates An optional vector of variable names that should be
#'   resampled to be used in the training and augmentation phases. If specified
#'   with \code{vars}, then the specified dataframe object is automatically
#'   filtered to just the \code{vars} and \code{covariates}.
#' @param train_prop A vector of numeric values from >0 and <1 indicating what
#'   proportion of the training set should randomly be injected with \code{NA}.
#'   Increase this value to inject more noise into the training stage. The
#'   default is set to \code{c(0.1, 0.2, 0.3)} for 10\%, 20\%, and 30\%
#'   injection of \code{NA}.
#' @param aug_prop A vector of numeric values from >0 and <1 indicating what
#'   proportion of the augmented set should be randomly replaced with the random
#'   forest predicted values. Increase this value to insert more of the random
#'   forest predicted values. The default is set to \code{c(0.3, 0.5, 0.7)} for
#'   30\%, 50\%, and 70\% replacement.
#' @param digits A numeric value indicating the number of digits to round to.
#' @param aug_n A numeric value indicating the number of observations should be
#'   augmented. The default is the same as the number of observations in the
#'   inputted data.
#' @param iter_n A numeric value indicating how many iterations of each training
#'   and augmenting combinations should be run. The default is 3.
#' @param tree_n A numeric value indicating the number of trees. The default
#'   number of trees is set to 100. To prevent or reduce chances of overfitting,
#'   lower the number of trees, particularly for larger sample sizes.
#' @param replace A logical value indicating whether the random selection of
#'   observations in the augmenting stage should be conducted with (\code{TRUE})
#'   or without (\code{FALSE}) replacement. The default is set to \code{FALSE}.
#'   \code{replace} cannot be \code{FALSE} if \code{aug_n} is greater than the
#'   number of observations in the inputted data. \code{replace} will
#'   automatically be changed to \code{TRUE} if this occurs.
#' @param bias_correction A logical value indicating whether the model accuracy
#'   calculation should aim for skewness and kurtosis bias correction
#'   (\code{TRUE}) or bias preservation (\code{FALSE}).
#' @param error_correction A logical value indicating whether the model accuracy
#'   calculation should apply a penalization (\code{TRUE}) if the average
#'   column-wide correlation difference between Augmented and Original data is
#'   positive (i.e., the augmented data shows greater average correlations
#'   between its variables than the original data) to reduce chances of Type I
#'   errors.
#' @param return_all_mods A logical value indicating whether to return all
#'   generated augmented models (\code{TRUE}) or only the best performing model
#'   (\code{FALSE}). The default is \code{FALSE}.
#' @param return_accuracy A logical value indicating whether to return a table
#'   of accuracy calculation measurements. The default is \code{TRUE}.
#'
#' @export
naugment <- function(
    
  data,
  vars = NULL,
  covariates = NULL,
  train_prop = c(0.1, 0.2, 0.3),
  aug_prop = c(0.3, 0.5, 0.7),
  aug_n = nrow(data),
  iter_n = 3,
  tree_n = 100,
  digits = 0,
  noise_value = 1,
  replace = FALSE,
  bias_correction = TRUE,
  error_correction = TRUE,
  mean_index_weight = 0.2,
  sd_index_weight = 0.2,
  cor_index_weight = 0.2,
  skew_index_weight = 0.2,
  kurtosis_index_weight = 0.2,
  return_all_mods = FALSE,
  return_accuracy = TRUE
  
) {
  
  # MESSAGES ----------------------------------------------------------------

  # If augmented data size exceeds original data size and `replace` is set to
  # FALSE, then change `replace` to TRUE
  if (aug_n > nrow(data) && isFALSE(replace)) {
    
    message(
      "Argument `replace` cannot be FALSE if aug_n < the number of observations 
      in the dataframe. Automatically changing `replace` to TRUE."
    )
    replace <- TRUE
    
  }
  
  
  # Sum up all index weights
  sum_index_weights <- sum(
    mean_index_weight,
    sd_index_weight,
    cor_index_weight,
    skew_index_weight,
    kurtosis_index_weight
  )
  
  # If index weights add up to be >1, rescale back to add to 1.
  if (sum_index_weights > 1) {
    
    message(
      "Index weights cannot sum to be greater than 1. Rescaling weights."
    )
    
    mean_index_weight <- mean_index_weight / sum_index_weights
    sd_index_weight <- sd_index_weight / sum_index_weights
    cor_index_weight <- cor_index_weight / sum_index_weights
    skew_index_weight <- skew_index_weight / sum_index_weights
    kurtosis_index_weight <- kurtosis_index_weight / sum_index_weights
    
  }
  
  
  
  # -- IF NEED TO FILTER DATA -- #
  
  if (!is.null(vars) && !is.null(covariates)) {
    
    data <- data %>% 
      dplyr::select(
        dplyr::all_of(
          c(covariates,
            vars)
        )
      )
    
  }
  

  # PARAMETERS --------------------------------------------------------------

  # Get all combinations of train_prop and aug_prop
  train_combos <- expand.grid(
    train = train_prop,
    aug = aug_prop,
    iter = seq(iter_n)
  )
  
  
  
  # ORIGINAL DATA METRICS ---------------------------------------------------

  org_metrics <- aug_metrics(data)
  
  

  # GENERATE AUGMENTED DATA -------------------------------------------------
  
  # Generate data
  augmented_data_list <- lapply(
    cli::cli_progress_along(
      seq(nrow(train_combos)),
      clear = FALSE,
      name = "Generating Augmented Data"
    ),
    function(x) {
      
      # Get training parameters
      train <- train_combos[[x,1]]
      augme <- train_combos[[x,2]]
      iter  <- train_combos[[x,3]]
      
      # Initialize training data with random missingness
      train_data <- inject_na(
        data,
        prop = train,
        vars = vars,
        method = "column"
      )
      
      # Train models
      missmodel <- missRanger::missRanger(
        data = train_data,
        num.trees = tree_n,
        verbose = 0,
        returnOOB = TRUE,
        data_only = FALSE,
        keep_forests = TRUE
      )
      
      # Get RF imputed data
      imputed_data <- round(
        x = missmodel[["data"]],
        digits = digits
      )
      
      # Resample as specified
      resample_data <- dplyr::sample_n(
        tbl = imputed_data,
        size = aug_n,
        replace = replace
      )
      
      # Inject random noise into imputed data
      noisy_data <- inject_noise(
        data = resample_data,
        noise_value = noise_value,
        vars = vars,
        digits = digits
      )
      
      # Create matrix of augmented RF predicted data
      augmented_mat <- lapply(
        names(data),
        function(n) {
          
          # Predict using trained missRanger RF models
          pred_mod <- predict(
            missmodel[["forests"]][[n]],
            data = noisy_data
          )
          
          # Get predicted models
          pred_vals <- pred_mod[["predictions"]]
          
          # Set up empty dataframe
          mat <- as.data.frame(
            matrix(
              nrow = aug_n,
              ncol = 1
            )
          ) %>% 
            
            # Rename
            dplyr::rename(
              !!sym(n) := "V1"
            )
          
          # Store data
          mat[[n]] <- pred_vals
          
          # Return
          return(mat)
          
        }
      )
      
      # Reduce into augmented dataframe
      augmented_df <- purrr::reduce(
        augmented_mat,
        cbind
      )
      
      # Create noisy augmented data
      noisy_augmented_data <- inject_na(
        data = noisy_data,
        prop = augme,
        vars = vars,
        method = "row"
      ) %>% 
        
        # Coalesce; replace NA with augmented dataframe
        dplyr::mutate(
          dplyr::across(
            .cols = dplyr::everything(),
            ~ dplyr::coalesce(
              ., 
              augmented_df[[dplyr::cur_column()]]
            )
          )
        ) %>% 
        
        # Round number
        round(digits = digits)
      
      
      # -- CALCULATE DATA EVALUATION METRICS -- #
      
      aug_metrics <- aug_metrics(
        noisy_augmented_data
      )
      
      # VARIABLE MEANS
      aug_mean_desc <- aug_fit(
        aug_vals = aug_metrics[["mean"]],
        org_vals = org_metrics[["mean"]]
      )
      
      
      # VARIABLE STANDARD DEVIATION
      aug_sd_desc <- aug_fit(
        aug_vals = aug_metrics[["sd"]],
        org_vals = org_metrics[["sd"]]
      )
      
      
      # VARIABLE CORRELATIONS
      aug_cor_desc <- aug_fit(
        aug_vals = aug_metrics[["cor"]],
        org_vals = org_metrics[["cor"]]
      )
      
      
      # VARIABLE SKEW
      aug_skew_desc <- aug_fit(
        aug_vals = aug_metrics[["skew"]],
        org_vals = org_metrics[["skew"]]
      )
      
      
      # VARIABLE KURTOSIS
      aug_kurt_desc <- aug_fit(
        aug_vals = aug_metrics[["kurtosis"]],
        org_vals = org_metrics[["kurtosis"]]
      )
      
      
      
      
      
      # -- MODEL META DATA -- #
      
      # Best iteration
      best_iter <- missmodel[["best_iter"]]
      
      # Best average OOB imputation error of best iteration
      best_oob <- missmodel[["mean_pred_errors"]][[best_iter]]
      
      # Compile meta data
      aug_meta_data <- list(
        iter = iter,
        train_prop = train,
        aug_prop = augme,
        best_rf_iter = best_iter,
        best_rf_oob = best_oob,
        rf_tree_n = tree_n,
        mean_accuracy = aug_mean_desc[["avg_accuracy"]],
        mean_sd_accuracy = aug_mean_desc[["sd_accuracy"]],
        sd_accuracy = aug_sd_desc[["avg_accuracy"]],
        sd_sd_accuracy = aug_sd_desc[["sd_accuracy"]],
        cor_accuracy = aug_cor_desc[["avg_accuracy"]],
        cor_sd_accuracy = aug_cor_desc[["sd_accuracy"]],
        cor_raw_dif = aug_cor_desc[["raw_differences"]],
        cor_sd_raw_dif = aug_cor_desc[["sd_raw_differences"]],
        skew_differences = aug_skew_desc[["differences"]],
        skew_sd_differences = aug_skew_desc[["sd_differences"]],
        skew_distance = aug_skew_desc[["distance"]],
        skew_sd_distance = aug_skew_desc[["sd_distance"]],
        kurt_differences = aug_kurt_desc[["differences"]],
        kurt_sd_differences = aug_kurt_desc[["sd_differences"]],
        kurt_distance = aug_kurt_desc[["distance"]],
        kurt_sd_distance = aug_kurt_desc[["sd_distance"]]
      )
      
      aug_output <- list(
        aug_data = noisy_augmented_data,
        meta_data = aug_meta_data
      )
      
      # Return
      return(aug_output)
      
    }
  )
  

  # Name models
  names(augmented_data_list) <- sprintf("model_%01d",
                                        seq(length(augmented_data_list)))
  
  # Get all training parameters
  train_col <- unlist(
    lapply(
      augmented_data_list,
      function(x) {
        x[["meta_data"]][["train_prop"]]
      }
    )
  )
  
  # Get all training parameters
  aug_col <- unlist(
    lapply(
      augmented_data_list,
      function(x) {
        x[["meta_data"]][["aug_prop"]]
      }
    )
  )
  
  # Get all training iterations
  iter_col <- unlist(
    lapply(
      augmented_data_list,
      function(x) {
        x[["meta_data"]][["iter"]]
      }
    )
  )
  
  # Get all mean difference metrics
  mean_col <- unlist(
    lapply(
      augmented_data_list,
      function(x) {
        x[["meta_data"]][["mean_accuracy"]]
      }
    )
  )
  
  mean_sd <- unlist(
    lapply(
      augmented_data_list,
      function(x) {
        x[["meta_data"]][["mean_sd_accuracy"]]
      }
    )
  )
  
  
  # Get all SD difference metrics
  sd_col <- unlist(
    lapply(
      augmented_data_list,
      function(x) {
        x[["meta_data"]][["sd_accuracy"]]
      }
    )
  )
  
  sd_sd <- unlist(
    lapply(
      augmented_data_list,
      function(x) {
        x[["meta_data"]][["sd_sd_accuracy"]]
      }
    )
  )
  
  
  # Get all cor mean difference metrics
  cor_col <- unlist(
    lapply(
      augmented_data_list,
      function(x) {
        x[["meta_data"]][["cor_accuracy"]]
      }
    )
  )
  
  cor_sd <- unlist(
    lapply(
      augmented_data_list,
      function(x) {
        x[["meta_data"]][["cor_sd_accuracy"]]
      }
    )
  )
  
  cor_dif <- unlist(
    lapply(
      augmented_data_list,
      function(x) {
        x[["meta_data"]][["cor_raw_dif"]]
      }
    )
  )
  
  cor_sd_dif <- unlist(
    lapply(
      augmented_data_list,
      function(x) {
        x[["meta_data"]][["cor_sd_raw_dif"]]
      }
    )
  )
  
  
  # Get all skew differences and distances
  skew_dif <- unlist(
    lapply(
      augmented_data_list,
      function(x) {
        x[["meta_data"]][["skew_differences"]]
      }
    )
  )
  
  skew_sd_dif <- unlist(
    lapply(
      augmented_data_list,
      function(x) {
        x[["meta_data"]][["skew_sd_differences"]]
      }
    )
  )
  
  skew_dis <- unlist(
    lapply(
      augmented_data_list,
      function(x) {
        x[["meta_data"]][["skew_distance"]]
      }
    )
  )
  
  skew_sd_dis <- unlist(
    lapply(
      augmented_data_list,
      function(x) {
        x[["meta_data"]][["skew_sd_distance"]]
      }
    )
  )
  
  
  # Get all kurtosis differences and distances
  kurt_dif <- unlist(
    lapply(
      augmented_data_list,
      function(x) {
        x[["meta_data"]][["kurt_differences"]]
      }
    )
  )
  
  kurt_sd_dif <- unlist(
    lapply(
      augmented_data_list,
      function(x) {
        x[["meta_data"]][["kurt_sd_differences"]]
      }
    )
  )
  
  kurt_dis <- unlist(
    lapply(
      augmented_data_list,
      function(x) {
        x[["meta_data"]][["kurt_distance"]]
      }
    )
  )
  
  kurt_sd_dis <- unlist(
    lapply(
      augmented_data_list,
      function(x) {
        x[["meta_data"]][["kurt_sd_distance"]]
      }
    )
  )
  
  

  # COMPILE -----------------------------------------------------------------

  # -- MEAN INDEX CALCULATION -- #
  
  mean_weight <- 1 / (mean_sd / mean(mean_sd))
  
  mean_index <- mean_col * mean_weight
  
  mean_index <- mean_index / max(mean_index)
  
  
  
  # -- SD INDEX CALCULATION -- #
  
  sd_weight <- 1 / (sd_sd / mean(sd_sd))
  
  sd_index <- sd_col * sd_weight
  
  sd_index <- sd_index / max(sd_index)
  
  
  
  # -- CORRELATION INDEX CALCULATION -- #
  
  cor_weight <- 1 / (cor_sd / mean(cor_sd))
  
  cor_error_weight <- 1 / (cor_sd_dif / mean(cor_sd_dif))
  
  cor_index <- if (isTRUE(error_correction)) {
    (cor_col * cor_weight) + (-cor_dif * cor_error_weight)
  } else {
    cor_col * cor_weight
  }
  
  cor_index = cor_index / max(cor_index)
  
  
  
  # -- SKEW INDEX CALCULATION -- #
  
  skew_weight = if (isTRUE(bias_correction)) {
    1 / (skew_sd_dis / mean(skew_sd_dis))
  } else {
    1 / (skew_sd_dif / mean(skew_sd_dif))
  }
  
  skew_index <- if (isTRUE(bias_correction)) {
    exp(-(skew_dis - min(skew_dis))) * skew_weight
  } else {
    exp(-(skew_dif - min(skew_dif))) * skew_weight
  }
  
  skew_index <- skew_index / max(skew_index)
  
  
  
  # -- KURTOSIS INDEX CALCULATION -- #
  
  kurt_weight <- if (isTRUE(bias_correction)) {
    1 / (kurt_sd_dis / mean(kurt_sd_dis))
  } else {
    1 / (kurt_sd_dif / mean(kurt_sd_dif))
  }
  
  kurt_index <- if (isTRUE(bias_correction)) {
    exp(-(kurt_dis - min(kurt_dis))) * skew_weight
  } else {
    exp(-(kurt_dif - min(kurt_dif))) * skew_weight
  }
  
  kurt_index <- kurt_index / max(kurt_index)
  
  
  
  # -- OVERALL INDEX CALCULATION -- #
  
  global_index <- mean_index_weight * mean_index +
    sd_index_weight * sd_index +
    cor_index_weight * cor_index +
    skew_index_weight * skew_index +
    kurtosis_index_weight * kurt_index
  
  global_index <- global_index / max(global_index)
  
  
  
  # -- COMPILE INDEX SCORES -- #
  
  aug_comparisons <- data.frame(
    model = seq(length(augmented_data_list)),
    iter = iter_col,
    train_prop = train_col,
    aug_prop = aug_col,
    mean = mean_index,
    sd = sd_index,
    cor = cor_index,
    skew = skew_index,
    kurt = kurt_index,
    global = global_index
  )
  
  aug_comparisons <- aug_comparisons %>% 
    # Rank by accuracy
    dplyr::arrange(
      dplyr::desc(global)
    )

  
  # Get the best augmented model
  best_aug_mod <- aug_comparisons[1,"model"]
  
  # Compile return
  aug_return <- if (isTRUE(return_all_mods)) {
    
    # If return accuracy is TRUE
    if (isTRUE(return_accuracy)) {
      
      list(
        accuracy_table = aug_comparisons,
        augmented_data = augmented_data_list
      )
      
    } else {
      
      augmented_data_list
      
    }
    
  } else {
    
    # If return accuracy is TRUE
    if (isTRUE(return_accuracy)) {
      
      list(
        accuracy_table = aug_comparisons,
        augmented_data = augmented_data_list[[best_aug_mod]]
      )
      
    } else {
      
      augmented_data_list[[best_aug_mod]]
      
    }
    
  }
  
  
  # Return
  return(aug_return)
  
}