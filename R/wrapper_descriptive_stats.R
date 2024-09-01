#' Descriptive Statistics Wrapper
#'
#' @description A simple function for automating the calculation of general
#'   descriptive statistics, like means, skew, missingness, reliability, and
#'   correlation matrices. The 'engine' of the correlation matrix is derived
#'   from Stefan Engineering's code (Stefan Engineering, 2018).
#'
#' @param data A dataframe object. This should be a structured dataset where
#'   each column represents a variable and each row represents an observation.
#' @param composite_list A required \code{composite_list} object. Each name in
#'   the list represents a composite variable, and the corresponding vector
#'   contains the column names that are associated with the indicators
#'   comprising said composite variable.
#' @param weight The weighting schema to use in calculating composite scores.
#'   For a weighting schema within the Covariance Family, set \code{weight} to
#'   \code{"average"} for unweighted, \code{"correlation"} for
#'   correlation-weighted, and \code{"regression"} for regression-weighted. For
#'   a weighting schema within the Standard Deviation (SD) Family, set
#'   \code{weight} to \code{"sd_upweight"} to upweight SDs and
#'   \code{"sd_downweight"} to downweight SDs. For a weighting schema within the
#'   Median Family, set \code{weight} to \code{"median"} to calculate the
#'   composite score as the median score, \code{"median_decay"} for
#'   distance-to-median weighting with the decay function, and
#'   \code{"median_gauss"} for distance-to-median weighting with the gaussian
#'   function.
#' @param digits The decimal places for the metrics to be rounded to. Default is
#'   3. This argument is only relevant if \code{return_metrics = TRUE}.
#' @param decay_rate A numeric value reflecting the decay rate (i.e.,
#'   sensitivity) of the distance-to-median weighting schema. The default value
#'   is set to 0.5. This argument is only relevant if \code{weight =
#'   "median_decay"}.
#' @param sigma A numeric value reflecting the sigma value for the Gaussian
#'   function in the distance-to-median weighting schema. The default value is
#'   set to 0.5. This argument is only relevant if \code{weight =
#'   "median_gauss"}.
#' @param entropy A string value reflecting the mutual information entropy
#'   estimator from the \code{infotheo} package. Four estimators are available:
#'   \code{emp} to compute the entropy of the empirical probability
#'   distribution. Empirical entropy is suitable for simple calculations without
#'   corrections. \code{mm} applies an asymptotic bias-corrected estimator
#'   making it suitable for small sample sizes. \code{shrink} applies a
#'   shrinkage estimate of the Dirichlet probability distribution to provide a
#'   stable estimate useful for small sample sizes or sparse data. \code{sg}
#'   applies a Schurmann-Grassberger estimate of the Dirichlet probability
#'   distribution to serve as an alternative to the Shrinkage approach.
#' @param nmi_method A string value reflecting the method used for calculating
#'   Normalized Mutual Information (NMI) values. \code{"average"} will normalize
#'   MI values using the average entropies of variables A and B.
#'   \code{"geometric"} will normalize MI values using the geometric mean of
#'   entropies of variables A and B.
#' @param return_metrics Logic to determine whether to return reliability and
#'   validity metrics. Set to \code{TRUE} for a list of dataframes with
#'   reliability and validity metrics.
#' @param type A string value indicating the type of correlations to compute.
#'   Specify \code{type = "pearson"} to compute Pearson's r correlation
#'   (default). Specify \code{"spearman"} to compute Spearman's correlation.
#' @param p_thresholds A vector of length 3 indicating the p-value thresholds
#'   for \code{*}, \code{**}, and \code{***} stars.
#' @param p_stars A logical indicating whether to have p-value stars
#'   (\code{TRUE}) or not (\code{FALSE}).
#' @param msd_position A string value indicating whether to have the M and SD of
#'   the variables on the left (\code{"left"}) or bottom of the correlation
#'   matrix (\code{"bottom"}).
#' @param name An optional string denoting the name of the study/analysis.
#'
#' @importFrom magrittr %>%
#'
#' @returns A list of dataframes containing the general descriptives of all
#'   variables in the dataset from \code{psych::describe()} and variable
#'   missingness information (Revelle, 2023). If the varlist was specified of
#'   latent or composite variables and their indicators, then three additional
#'   dataframes are returned. These include a new dataset with the calculated
#'   latent or composite variables using the specified indicators, a correlation
#'   matrix of the latent or composite variables, and a measurement reliability
#'   table of the latent or composite variables. The latent or composite
#'   variables are calculated as the unweighted means of all indicators. The
#'   measurement reliability table returns the Cronbach's alpha and McDonald's
#'   omega as calculated by the jamovi package's \code{jmv::reliability()}
#'   function (Selker et al., 2022).
#'
#' @examples
#' varlist <- list(anxiety = c("anx_1", "anx_2", "anx_3"),
#'                 sleep = c("slp_1", "slp_2"),
#'                 life_sat = c("lsat_1", "lsat_2", "lsat_3"))
#' desc_wrapper(data, varlist, digits = 3)
#'
#' @references William Revelle (2023). psych: Procedures for Psychological,
#'   Psychometric, and Personality Research. Northwestern University, Evanston,
#'   Illinois. R package version 2.3.3,
#'   \url{https://CRAN.R-project.org/package=psych}.
#'
#'   Selker R, Love J, Dropmann D, Moreno V (2022). jmv: The 'jamovi' Analyses.
#'   R package version 2.3.4, \url{https://CRAN.R-project.org/package=jmv}.
#'
#'   Stefan Engineering (2018). Create an APA style correlation table with R.
#'   \url{https://stefaneng.github.io/apa_correlation_table/}.
#'
#' @export
wrap_desc <- function(
    data = .,
    composite_list = NULL,
    weight = "correlation",
    decay_rate = 0.5,
    sigma = 0.5,
    entropy = "emp",
    nmi_method = "geometric",
    return_metrics = TRUE,
    digits = 3,
    type = "pearson",
    p_thresholds = c(0.05, 0.01, 0.001),
    p_stars = TRUE,
    msd_position = "left",
    name = NULL
){
  
  # If composite list is given, then get reliability metrics for each composite
  
  if(!is.null(composite_list)){
    
    # Composite score
    cs <- cscore::composite_score(
      
      data = data,
      composite_list = composite_list,
      weight = weight,
      decay_rate = decay_rate,
      sigma = sigma,
      return_metrics = return_metrics,
      digits = digits
      
    )
    
    # Get data
    cs_df <- cs[["data"]]
    
    # Get loadings & weights
    cs_lw <- cs[["metrics"]]
    
    # Get reliability & validity
    cs_rl <- cs[["validity"]]
    
    # Composite order
    cs_od <- composite_list[["order"]]
    
    # Get correlation matrix
    cor_mat <- cor_matrix(
      
      data = cs_df,
      vars = cs_od,
      digits = digits,
      type = type,
      p_thresholds = p_thresholds,
      p_stars = p_stars,
      msd_position = msd_position
      
    )
    
  }
  

  # Descriptive for data
  data_desc <- data.frame(
    psych::describe(data)
  ) %>% 
    tibble::rownames_to_column(var = "variable")
  
  # Data missingness
  data_missing <- data %>%
    dplyr::summarise_all(
      list(~sum(is.na(.))/nrow(data))
    ) %>%
    tidyr::gather(key = "variable",
                  value = "missingness") %>% 
    dplyr::mutate(n_total = nrow(data),
                  n_missing = n_total*missingness)
  
  
  # Report results
  if(!is.null(composite_list)){
    
    sheets <- list(
      "desc" = data_desc,
      "miss" = data_missing,
      "load" = cs_lw,
      "reli" = cs_rl,
      "corr" = cor_mat
    )
    
  } else {
    
    sheets <- list(
      "desc" = data_desc,
      "miss" = data_missing
    )
    
  }
  
  # Return
  return(sheets)
  
}
