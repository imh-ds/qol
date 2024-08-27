
#' Augmented Data Evaluation Metrics
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
aug_metrics <- function(df) {
  
  # Get mean vector
  mean_vec <- df %>% 
    dplyr::summarise(
      dplyr::across(
        .cols = dplyr::everything(),
        ~mean(., na.rm = T)
      )
    )
  
  # Reduce vector
  mean_vec <- unlist(c(mean_vec))
  
  
  # Get SD vector
  sd_vec <- df %>% 
    dplyr::summarise(
      dplyr::across(
        .cols = dplyr::everything(),
        ~sd(., na.rm = T)
      )
    )
  
  # Reduce vector
  sd_vec <- unlist(c(sd_vec))
  
  
  # Get correlation matrix
  cor_mat <- cor(df,
                 use = "pairwise.complete.obs")
  diag(cor_mat) <- NA
  
  # Get correlation vector
  cor_vec <- as.data.frame(
    cor_mat
  ) %>% 
    dplyr::summarise(
      dplyr::across(
        .cols = dplyr::everything(),
        ~mean(., na.rm = T)
      )
    )
  
  # Reduce vector
  cor_vec <- unlist(c(cor_vec))
  
  
  # Get skew vectors
  skew_vec <- df %>% 
    dplyr::summarise(
      dplyr::across(
        .cols = dplyr::everything(),
        ~psych::skew(., na.rm = T)
      )
    )
  
  # Reduce vector
  skew_vec <- unlist(c(skew_vec))
  
  
  # Get kurtosis vectors
  kurt_vec <- df %>% 
    dplyr::summarise(
      dplyr::across(
        .cols = dplyr::everything(),
        ~psych::kurtosi(., na.rm = T)
      )
    )
  
  # Reduce vector
  kurt_vec <- unlist(c(kurt_vec))
  
  
  # Compile together
  metrics <- list(
    mean = mean_vec,
    sd = sd_vec,
    cor = cor_vec,
    skew = skew_vec,
    kurtosis = kurt_vec
  )
  
  return(metrics)
  
}