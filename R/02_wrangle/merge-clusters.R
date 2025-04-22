pacman::p_load(dplyr, tidyr, withr)

# Add the clustering and reduced variables to the main data frame
merge_clusters <- function(df_raw, df_red, clustering) {
  withr::local_options(list(warn = -1))
  
  new_cols <-
    df_red |>
    mutate(
      id = df_raw$id,
      cluster = 
        clustering$classification |> 
        case_match(1 ~ "B", 2 ~ "C", 3 ~ "A")
    ) |> 
    rename(spatial_span = span_spatial)
  
  df_new <-
    left_join(df_raw, new_cols, by = "id") |> 
    unite("subcluster", cluster, group, remove = FALSE) |> 
    mutate(
      cluster = factor(
        cluster,
        levels = c("A", "B", "C"),
        labels = c("A (Aphant.)", "B (Mixed)", "C (Control)")
      ),
      subcluster = factor(
        subcluster,
        levels = c("A_Aphantasic", "B_Aphantasic", "B_Control", "C_Control"),
        labels = c("A (Aphant.)", "B-Aphant.", "B-Control", "C (Control)")
      )
    ) |> 
    select(id, group, cluster, subcluster, age, everything())
  
  return(df_new)
}