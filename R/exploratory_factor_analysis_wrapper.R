#' EXPLORATORY FACTOR ANALYSIS WRAPPER
#' 
#' @description
#' Runs exploratory factor analysis (EFA) through the \code{psych} package's \code{fa()}
#' function. A simple wrapper that automates a general EFA for most needs.
#' For complex or custom settings, users are recommended to simply use the
#' \code{psych::fa} (Revelle, 2023). 
#' 
#' @param data A dataframe object. This should be a structured dataset where
#' each column represents a variable and each row represents an observation.
#' @param variables A required vector of column names representing their 
#' manifest variables.
#' @param dictionary variable dictionary
#' @param factors_n A single numeric value for the number of factors to
#' identify. Default is NULL where it automatically determines the optimal
#' number of factors.
#' @param rotation Defaults to "oblimin" for an oblique rotation. See
#' \code{help(psych::fa)} for other rotation options.
#' @param missing Defaults to NULL, and does not impute missing values. If
#' set to TRUE, specify impute option.
#' @param impute default to "none". Specify impute option "mean" or "median" to replace missing values.
#' @param plot default to FALSE. If set to TRUE, returns elbow plot.
#' 
#' @return The exploratory factor analysis output, a dataframe of the items
#' sorted from highest to lowest factor loading on their respective factors,
#' and a list of additional outputs.
#' 
#' @examples
#' variables = c("item1", "item2", "item3", "item4")
#' efa_wrapper(data, variables, plot = TRUE)
#' 
#' @references William Revelle (2023). psych: Procedures for Psychological,
#' Psychometric, and Personality Research. Northwestern University, Evanston,
#' Illinois. R package version 2.3.3,
#' \url{https://CRAN.R-project.org/package=psych}.
#' 
#' @export
efa_wrapper = function(data = .,
                       variables,
                       dictionary = NULL,
                       factors_n = NULL,
                       rotation = "oblimin",
                       missing = FALSE,
                       impute = "none",
                       plot = FALSE){
  
  data <- data %>% 
    dplyr::select(all_of(variables))
  
  if(is.null(factors_n)){
    
    efa <- psych::fa(data,
                     fm = "minres",
                     nfactors = psych::fa.parallel(data,
                                                   fm = "minres",
                                                   plot = plot)$nfact,
                     rotate = rotation,
                     missing = missing,
                     impute = impute)
    
  } else {
    
    efa <- psych::fa(data,
                     fm = "minres",
                     nfactors = factors_n,
                     rotate = rotation,
                     missing = missing,
                     impute = impute)
    
  }
  
  efa_sort <- as.data.frame(psych::fa.sort(efa)$loadings[]) %>%
    rownames_to_column(var = "Item") %>%
    inner_join(as.data.frame(efa$uniqueness) %>%
                 rownames_to_column(var = "Item")) %>% 
    rename(Uniqueness = "efa$uniqueness")
  
  if(!is.null(dictionary)){
    
    efa_sort <- efa_sort %>%
      inner_join(as.data.frame(dictionary)) %>% 
      dplyr::select(Item, Label, everything())
    
  }
  
  fa_scores <- list("SCORES" = efa$scores,
                    "WEIGHTS" = efa$weights,
                    "R_SCORES" = efa$r.scores,
                    "R2" = efa$R2)
  
  sheets <- list("EFA" = efa,
                 "EFA_sorted" = efa_sort,
                 "EFA_scores" = fa_scores)
  
  return(sheets)
  
}