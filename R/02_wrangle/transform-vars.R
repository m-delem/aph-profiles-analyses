if (!requireNamespace("pacman")) install.packages("pacman")
pacman::p_load(dplyr, mclust, scales)

#' Scale quantitative variables to a defined range
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


#' Reduce the number of variables to prepare for clustering
#'
#' @param df The main data frame
#'
#' @return A data frame with the reduced variables
#' 
reduce_vars <- function(df){
  df_reduced <- 
    df |> 
    scale_vars() |> 
    select(vviq:psiq_vis, score_raven:span_digit, score_similarities) |> 
    mutate(
      # merging the normalized vviq, osviq-o, and psiq scores
      visual_imagery = round(
        (16*vviq + 15*osivq_o + 3*psiq_vis)/34,  
        digits = 3
      ),
      # merging SRI and Raven relative to their number of items
      non_verbal_reasoning = round(
        (30*score_sri + 18*score_raven)/48,
        digits = 3
      ),
      .keep = "unused"
    ) |> 
    # reordering
    select(
      visual_imagery,
      spatial_imagery = osivq_s,
      verbal_strategies = osivq_v,
      non_verbal_reasoning,
      verbal_reasoning = score_similarities,
      span_spatial = span_spatial,
      span_digit = span_digit
    )
  
  return(df_reduced)
}


#' Cluster the reduced variables
#'
#' @param df The main data frame
#'
#' @return A clustering object
#' 
cluster_vars <- function(df) {
  clustering <- Mclust(reduce_vars(df), G = 3, verbose = FALSE)
  return(clustering)
}


#' Add the clustering and reduced variables to the main data frame
#'
#' @param df         The main data frame
#' @param clustering The clustering object
#'
#' @return A data frame with the clustering classification and reduced variables
#' 
add_cluster_vars <- function(df, clustering) {
  new_cols <-
    df |> 
    reduce_vars() |> 
    mutate(
      id = df$id,
      cluster = clustering$classification |> 
        case_match(1 ~ "B", 2 ~ "A", 3 ~ "C") |> 
        factor(levels = c("A", "B", "C"))
    ) |> 
    rename_with(~ paste0(., "_std"), contains("span"))
  
  df_new <-
    left_join(df, new_cols, by = "id") |> 
    select(id, group, cluster, age, everything())
  
  return(df_new)
}


rename_vars <- function(df) {
  quantitative_vars <- c(
    "VVIQ" = "vviq",
    "OSIVQ-Object" = "osivq_o",
    "OSIVQ-Spatial" = "osivq_s",
    "OSIVQ-Verbal"  = "osivq_v",
    "Psi-Q Vision"  = "psiq_vis",
    "Psi-Q Audition" = "psiq_aud",
    "Psi-Q Smell" = "psiq_od",
    "Psi-Q Taste" = "psiq_gout",
    "Psi-Q Touch" = "psiq_tou",
    "Psi-Q Sensations" = "psiq_sens",
    "Psi-Q Feelings" = "psiq_feel",
    "Raven matrices" = "score_raven",
    "SRI" = "score_sri",
    "Digit span"   = "span_digit",
    "Spatial span" = "span_spatial",
    "WCST" = "wcst_accuracy",
    "Similarities test" = "score_similarities",
    "Reading comprehension" = "score_comprehension"
  )
  
  df <- df |> rename(any_of(quantitative_vars))
  
  return(df)
}