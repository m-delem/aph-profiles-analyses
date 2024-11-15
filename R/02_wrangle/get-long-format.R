if (!requireNamespace("pacman")) install.packages("pacman")
pacman::p_load(dplyr, tidyr)

#' Transform the main data frame to long format
#'
#' @param df The main data frame
#' 
get_long_format <- function(df){
  
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
  
  df_long <- 
    df |>
    pivot_longer(
      any_of(c(
        contains("vviq"),
        contains("osivq"),
        contains("psiq"),
        contains("score"),
        contains("span"),
        contains("wcst")
      )),
      names_to = "Variable", 
      values_to = "value"
    ) |> 
    mutate(
      Variable = fct_inorder(Variable),
      Variable = fct_recode(Variable, !!!quantitative_vars)
    ) |> 
    rename_with(str_to_title, any_of(c(
      "age", "sex", "group", 
      "education", "field", "occupation"
      ))
    )
  
  return(df_long)
}
