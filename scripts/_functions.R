# functions shared across scripts are placed here


# Shorthand for summing item columns --------------------------------------

sum_items <- function(name){rowSums(across(starts_with(name)), na.rm = TRUE)}


# Scaling the quantitative variables --------------------------------------

scale_quantitative_vars <- function(df){
  
  min <- 0
  max <- 1
  
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
      score_comprehension = rescale(score_comprehension, c(min, max), c(0,40)),
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


# Transforming the main table to long format ------------------------------

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
