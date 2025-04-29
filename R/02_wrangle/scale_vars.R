# Scale original quantitative variables to a defined range
scale_vars <- function(df, min = 0, max = 1){
  
  df_scaled <- 
    df |> 
    dplyr::mutate(
      dplyr::across(
        tidyselect::starts_with("age"),
        ~ scales::rescale(., c(min, max), c(min(age),max(age)))
      ),
      dplyr::across(
        tidyselect::contains("vviq"),    
        ~ scales::rescale(., c(min, max), c(16,80))
      ),
      dplyr::across(
        tidyselect::contains("osivq"),   
        ~ scales::rescale(., c(min, max), c(15, 75))
      ),
      dplyr::across(
        tidyselect::contains("psiq"),    
        ~ scales::rescale(., c(min, max), c(1, 10))
      ),
      dplyr::across(
        tidyselect::contains("raven"),   
        ~ scales::rescale(., c(min, max), c(0, 36))
      ),
      dplyr::across(
        tidyselect::contains("sri"),     
        ~ scales::rescale(., c(min, max), c(0, 30))
      ),
      dplyr::across(
        tidyselect::contains("span"),    
        ~ scales::rescale(., c(min, max), c(0, max(.)))
      ),
      dplyr::across(
        tidyselect::contains("wcst"),    
        ~ scales::rescale(., c(min, max), c(0, 100))
      ),
      dplyr::across(
        tidyselect::contains("similar"), 
        ~ scales::rescale(., c(min, max), c(0, 36))
      ),
      dplyr::across(
        tidyselect::contains("comprehension"), 
        ~ scales::rescale(., c(min, max), c(0, max(.) + 3))
      ),
      dplyr::across(tidyselect::any_of(
        c(
          tidyselect::contains("age"), 
          tidyselect::contains("vviq"), 
          tidyselect::contains("osivq"), 
          tidyselect::contains("psiq"),
          tidyselect::contains("score"), 
          tidyselect::contains("span"), 
          tidyselect::contains("wcst")
        )
      ),
      ~ round(., 3)
      )
    )
  
  return(df_scaled)
}
