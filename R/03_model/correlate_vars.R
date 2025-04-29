here::here("R/02_wrangle/scale_vars.R") |> source()

# Correlate the original variables with a chosen method and correction
correlate_vars <- function(
    df, 
    method = "pearson",
    partial = TRUE, 
    correction = "bonferroni"
) {
  correlations <- 
    df |>
    scale_vars() |> 
    dplyr::select(vviq:score_comprehension) |> 
    correlation::correlation(
      method   = method,
      partial  = partial,
      p_adjust = correction
    ) |> 
    dplyr::mutate(
      dplyr::across(
        c(Parameter1, Parameter2),
        ~dplyr::case_match(
          .x,
          "vviq" ~ "VVIQ",
          "osivq_o" ~ "OSIVQ\nObject",
          "osivq_s" ~ "OSIVQ\nSpatial",
          "osivq_v" ~ "OSIVQ\nVerbal",
          "psiq_vis" ~ "Psi-Q\nVisual",
          "psiq_aud" ~ "Psi-Q\nAudition",
          "psiq_od" ~ "Psi-Q\nSmell",
          "psiq_gout" ~ "Psi-Q\nTaste",
          "psiq_tou" ~ "Psi-Q\nTouch",
          "psiq_sens" ~ "Psi-Q\nSensations",
          "psiq_feel" ~ "Psi-Q\nFeelings",
          "score_raven" ~ "Raven\nMatrices",
          "score_sri" ~ "SRI",
          "span_spatial" ~ "Spatial\nspan",
          "span_digit" ~ "Digit\nspan",
          "wcst_accuracy" ~ "WCST",
          "score_similarities" ~ "Similarities",
          "score_comprehension" ~ "Reading"
        )
      )
    )
  
  return(correlations)
}