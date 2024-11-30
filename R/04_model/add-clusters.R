# if (!requireNamespace("pacman")) install.packages("pacman")
pacman::p_load(dplyr, tidyr)

add_clusters <- function(df, cluster_results) {
  df_with_clusters <- 
    cluster_results |> 
    unnest(c(selection, id)) |>
    select(id, method, majority) |> 
    pivot_wider(names_from = method, values_from = majority) |> 
    rename(any_of(c("DM" = "DiffusionMaps", "Iso" = "Isomap"))) |> 
    rename_with(~paste0("cluster_", .), !id) |> 
    left_join(df, by = "id")
  
  return(df_with_clusters)
}