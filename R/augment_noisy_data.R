
#' Augment Noisy Data
#'
#' @param data 
#' @param tree_n 
#' @param initial_n_prop 
#' @param digits 
#' @param aug_n 
#' @param replace 
#'
#' @export
naugment <- function(
    
  data,
  tree_n = 150,
  initial_n_prop = 0.1,
  digits = 0,
  aug_n = nrow(data),
  replace = FALSE
  
) {
  
  # Initialize training data with random missingness
  train_data <- qol::inject_na(
    data,
    prop = initial_n_prop
  )
  
  # Train models
  missmodel <- missRanger::missRanger(
    
    data = train_data,
    num.trees = tree_n,
    verbose = 1,
    data_only = FALSE,
    keep_forests = TRUE,
    returnModels = TRUE
    
  )
  
  # Get RF imputed data
  imputed_data <- missmodel$data
  
  # Inject random noise into imputed data
  noisy_data <- round(
    qol::inject_noise(data = imputed_data)
  )
  
  # Create matrix of augmented RF predicted data
  augmented_mat <- lapply(
    names(data),
    function(n) {
      
      # Predict using trained missRanger RF models
      pred_mod <- predict(missmodel[["forests"]][[n]],
                          data = noisy_data)
      
      # Get predicted models
      pred_vals <- pred_mod[["predictions"]]
      
      # Set up empty dataframe
      mat <- as.data.frame(
        
        matrix(
          nrow = nrow(df),
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
  augmented_df <- purrr::reduce(augmented_mat,
                                cbind)
  
  # Create noisy augmented data
  noisy_augmented_data <- qol::inject_na(
    data = noisy_data,
    prop = .5) %>% 
    
    # Inject random noise
    qol::inject_noise() %>% 
    
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
    
    # Round to whole number
    round(digits = digits)
  
  
  # Resample as specified
  final_aug_data <- dplyr::sample_n(
    tbl = noisy_augmented_data,
    size = aug_n,
    replace = replace)
  
  # Return
  return(final_aug_data)
  
}