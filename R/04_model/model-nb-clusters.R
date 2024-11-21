# if (!requireNamespace("pacman")) install.packages("pacman")
pacman::p_load(dplyr, parameters, tidyr)
# source(here("R/02_wrangle/transform-vars.R"))

model_nb_clusters <- function(df, selected_vars) {
  # computing indices for all sequential dataframes starting from 57 subjects
  nb_clusters_evolution <- tribble(
    ~ n, ~ df_seq, ~ n_Clusters
  )
  
  nb_temp <- nb_clusters_evolution
  
  for(i in seq(57, nrow(df), by = 1)){
    nb_temp <- tribble(
      ~ n, ~ df_seq, ~ n_Clusters,
      i,  df |> reduce_vars() |> select(any_of(selected_vars)) |> slice(1:i), 0
    )
    nb_clusters_evolution <- bind_rows(nb_clusters_evolution, nb_temp)
  }
  
  nb_clusters_evolution <-
    nb_clusters_evolution |> 
    rowwise() |> 
    mutate(n_Clusters = list(n_clusters(df_seq))) |> 
    unnest(n_Clusters) |> 
    group_by(n, n_Clusters) |> 
    count(name = "number_of_methods") |> 
    pivot_wider(
      names_from = n_Clusters,
      values_from = number_of_methods,
      values_fill = 0
    ) |> 
    select(n, "1","2","3","4","5","6","7","8","9")
  
  return(nb_clusters_evolution)
}