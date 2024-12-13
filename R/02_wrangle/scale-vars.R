# if (!requireNamespace("pacman")) install.packages("pacman")
pacman::p_load(dplyr, scales)

# Scale original quantitative variables to a defined range
scale_vars <- function(df, min = 0, max = 1){
  
  df_scaled <- 
    df |> 
    mutate(
      across(
        starts_with("age"),  ~ rescale(., c(min, max), c(min(age),max(age)))
      ),
      across(
        contains("vviq"),    ~ rescale(., c(min, max), c(16,80))
      ),
      across(
        contains("osivq"),   ~ rescale(., c(min, max), c(15, 75))
      ),
      across(
        contains("psiq"),    ~ rescale(., c(min, max), c(1, 10))
      ),
      across(
        contains("raven"),   ~ rescale(., c(min, max), c(0, 36))
      ),
      across(
        contains("sri"),     ~ rescale(., c(min, max), c(0, 30))
      ),
      across(
        contains("span"),    ~ rescale(., c(min, max), c(0, max(.)))
      ),
      across(
        contains("wcst"),    ~ rescale(., c(min, max), c(0, 100))
      ),
      across(
        contains("similar"), ~ rescale(., c(min, max), c(0, 36))
      ),
      across(
        contains("comprehension"), ~ rescale(., c(min, max), c(0, max(.) + 3))
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
    )
  
  return(df_scaled)
}
