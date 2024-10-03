# functions shared across scripts are placed here

get_long_format <- function(df){
  
  quantitative_vars <- c(
    "vviq",
    "osivq_o",
    "osivq_s",
    "osivq_v",
    "psiq_vis",
    "psiq_aud",
    "Psi-Q Smell" = "psiq_od",
    "Psi-Q Taste" = "psiq_gout",
    "Psi-Q Touch"  = "psiq_tou",
    "Psi-Q Sensations" = "psiq_sens",
    "Psi-Q Feelings" = "psiq_feel",
    "Raven matrices"   = "score_raven",
    "SRI" = "score_sri",
    "Digit span" = "span_digit",
    "Spatial span" = "span_spatial",
    "WCST" = "wcst_accuracy",
    "Similitudes test" = "score_similitudes",
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
        contains("span")
      )),
      names_to = "Variable", 
      values_to = "value")
    
}