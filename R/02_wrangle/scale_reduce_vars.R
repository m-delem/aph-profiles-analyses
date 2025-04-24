library(dplyr)

# Reduce the number of variables to prepare for clustering
scale_reduce_vars <- function(df, min = 0, max = 1){
  df_reduced <- 
    df |> 
    mutate(
      across(
        matches("age"),     ~ scales::rescale(., c(min, max), c(min(age),max(age)))
      ),
      across(
        contains("vviq"),    ~ scales::rescale(., c(min, max), c(16,80))
      ),
      across(
        contains("osivq"),   ~ scales::rescale(., c(min, max), c(15, 75))
      ),
      across(
        contains("psiq"),    ~ scales::rescale(., c(min, max), c(1, 10))
      ),
      across(
        contains("raven"),   ~ scales::rescale(., c(min, max), c(0, 36))
      ),
      across(
        contains("sri"),     ~ scales::rescale(., c(min, max), c(0, 30))
      ),
      across(
        contains("span"),    ~ scales::rescale(., c(min, max), c(0, max(.)))
      ),
      across(
        contains("wcst"),    ~ scales::rescale(., c(min, max), c(0, 100))
      ),
      across(
        contains("similar"), ~ scales::rescale(., c(min, max), c(0, 36))
      ),
      across(
        contains("comprehension"), ~ scales::rescale(., c(min, max), c(0, max(.) + 3))
      ),
      across(any_of(
        c(
          contains("age"), 
          contains("vviq"), contains("osivq"), contains("psiq"),
          contains("score"), contains("span"), contains("wcst")
        )
      ),
      ~ round(., 3)
      )
    ) |> 
    mutate(
      # merging the normalized vviq, osviq-o, and psiq visual scores
      visual_imagery = round(
        (16*vviq + 15*osivq_o + 3*psiq_vis)/34,  
        digits = 3
      ),
      # merging the normalized psiq scores on the other modalities
      sensory_imagery = round(
        (psiq_od + psiq_gout + psiq_tou + psiq_sens + psiq_feel)/5,
        digits = 3
      ),
      # merging SRI and OSIVQ-S relative to their number of items
      spatial_imagery = round(
        (30*score_sri + 15*osivq_s)/45,
        digits = 3
      ),
      # merging raven and digit span according to partial correlations
      fluid_intelligence = round(
        (score_raven + span_digit)/2,
        digits = 3
      ),
      # merging SRI and Raven relative to their number of items (old tests)
      non_verbal_reasoning = round(
        (30*score_sri + 18*score_raven)/48,
        digits = 3
      )
    ) |> 
    select(
      visual_imagery,
      auditory_imagery = psiq_aud,
      sensory_imagery,
      spatial_imagery,
      verbal_strategies = osivq_v,
      fluid_intelligence,
      # non_verbal_reasoning,
      verbal_reasoning = score_similarities,
      span_spatial
    )
  
  return(df_reduced)
}
