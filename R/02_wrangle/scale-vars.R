# if (!requireNamespace("pacman")) install.packages("pacman")
pacman::p_load(dplyr, scales)

#' Scale original quantitative variables to a defined range
#'
#' @description
#' This function scales all quantitative variables in the data from their
#' original range, defined here by hand, to a new range defined by the user.
#' 
#' @param df The main data frame
#' @param min The minimum value of the new range
#' @param max The maximum value of the new range
#'
#' @return A data frame with the scaled variables
#' 
scale_vars <- function(df, min = 0, max = 1){
  
  df_scaled <- 
    df |> 
    mutate(
      age  = rescale(age,  c(min, max), c(min(age),max(age))),
      vviq = rescale(vviq, c(min, max), c(16,80)),
      score_raven  = rescale(score_raven, c(min, max), c(0,18)),
      score_sri    = rescale(score_sri,   c(min, max), c(0,30)),
      span_spatial = rescale(span_spatial, c(min, max), c(0,max(span_spatial))),
      span_digit   = rescale(span_digit,   c(min, max), c(0,max(span_digit))),
      wcst_accuracy = rescale(wcst_accuracy, c(min, max), c(0,100)),
      score_similarities = rescale(score_similarities,   c(min, max), c(0,36)),
      score_comprehension = rescale(
        score_comprehension, 
        c(min, max), 
        c(0,max(score_comprehension) + 3)),
      across(contains("osivq"), ~ rescale(., c(min, max), c(15, 75))),
      across(contains("psiq"),  ~ rescale(., c(min, max), c(1, 10))),
      across(any_of(c(
        contains("age"), contains("vviq"), contains("osivq"), contains("psiq"), 
        contains("score"), contains("span"), contains("wcst")
      )),
      ~ round(., 3)
      )
    )
  
  return(df_scaled)
}
