# if (!requireNamespace("pacman")) install.packages("pacman")
pacman::p_load(dplyr, tidyr, withr)

#' Transform the main data frame to long format
#'
#' @param df The main data frame
#' 
get_long_format <- function(df){
  withr::local_options(list(warn = -1))
  
  vars <- c(
    # Original scores
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
    "Similarities test" = "score_similarities",
    # Reduced variables
    "Visual imagery" = "visual_imagery",
    "Auditory imagery" = "auditory_imagery",
    "Sensory imagery" = "sensory_imagery",
    "Spatial imagery" = "spatial_imagery",
    "Verbal strategies" = "verbal_strategies",
    "Fluid\nintelligence" = "fluid_intelligence",
    "Non-verbal\nreasoning" = "non_verbal_reasoning",
    "Verbal reasoning" = "verbal_reasoning",
    "Spatial span" = "spatial_span",
    # Complex tasks
    "WCST" = "wcst_accuracy",
    "Reading\ncomprehension" = "score_comprehension"
  )
  
  df_long <- 
    df |>
    pivot_longer(
      any_of(c(
        # original
        contains("vviq"),
        contains("osivq"),
        contains("psiq"),
        contains("raven"),
        contains("sri"),
        matches("span_digit"),
        matches("span_spatial"),
        contains("similarities"),
        # reduced
        contains("imagery"),
        contains("strategies"),
        contains("intelligence"),
        contains("reasoning"),
        matches("spatial_span"),
        # complex
        contains("wcst"),
        contains("comprehension")
      )),
      names_to = "Variable", 
      values_to = "value"
    ) |> 
    mutate(
      Variable = fct_inorder(Variable),
      Variable = fct_recode(Variable, !!!vars)
    ) |> 
    rename_with(str_to_title, any_of(c(
      "age", "sex", "group", "cluster", "subcluster",
      "education", "field", "occupation"
    ))
    )
  
  return(df_long)
}
