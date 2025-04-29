# Transform the main data frame to long format
get_longer <- function(df){
  withr::local_options(list(warn = -1))
  
  vars <- c(
    # Original scores
    "VVIQ"                   = "vviq",
    "OSIVQ-Object"           = "osivq_o",
    "OSIVQ-Spatial"          = "osivq_s",
    "OSIVQ-Verbal"           = "osivq_v",
    "Psi-Q Vision"           = "psiq_vis",
    "Psi-Q Audition"         = "psiq_aud",
    "Psi-Q Smell"            = "psiq_od",
    "Psi-Q Taste"            = "psiq_gout",
    "Psi-Q Touch"            = "psiq_tou",
    "Psi-Q Sensations"       = "psiq_sens",
    "Psi-Q Feelings"         = "psiq_feel",
    "Raven matrices"         = "score_raven",
    "SRI"                    = "score_sri",
    "Digit span"             = "span_digit",
    "Spatial span"           = "span_spatial",
    "Similarities test"      = "score_similarities",
    # Reduced variables
    "Visual imagery"         = "visual_imagery",
    "Auditory imagery"       = "auditory_imagery",
    "Sensory imagery"        = "sensory_imagery",
    "Spatial imagery"        = "spatial_imagery",
    "Verbal strategies"      = "verbal_strategies",
    "Raven +\nDigit Span"    = "fluid_intelligence",
    "Non-verbal\nreasoning"  = "non_verbal_reasoning",
    "Verbal reasoning"       = "verbal_reasoning",
    "Spatial span std."      = "spatial_span",
    # Complex tasks
    "WCST"                   = "wcst_accuracy",
    "Reading\ncomprehension" = "score_comprehension"
  )
  
  df_long <- 
    df |>
    tidyr::pivot_longer(
      tidyselect::any_of(c(
        # original
        tidyselect::contains("vviq"),
        tidyselect::contains("osivq"),
        tidyselect::contains("psiq"),
        tidyselect::contains("raven"),
        tidyselect::contains("sri"),
        tidyselect::matches("span_digit"),
        tidyselect::matches("span_spatial"),
        tidyselect::contains("similarities"),
        # reduced
        tidyselect::contains("imagery"),
        tidyselect::contains("strategies"),
        tidyselect::contains("intelligence"),
        tidyselect::contains("reasoning"),
        tidyselect::ends_with("spatial_span"),
        # complex
        tidyselect::contains("wcst"),
        tidyselect::contains("comprehension")
      )),
      names_to = "Variable", 
      values_to = "value"
    ) |> 
    dplyr::mutate(
      Variable = forcats::fct_inorder(Variable),
      Variable = forcats::fct_recode(Variable, !!!vars)
    ) |> 
    dplyr::rename_with(
      stringr::str_to_title, 
      tidyselect::any_of(c(
        "age", "sex", "group", "cluster", "subcluster",
        "education", "field", "occupation"
      ))
    )
  
  return(df_long)
}
