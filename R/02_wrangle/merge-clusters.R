# Add the clustering and reduced variables to the main data frame
merge_clusters <- function(df_raw, df_reduced, clustering) {
  withr::local_options(list(warn = -1))
  
  new_cols <-
    df_reduced |>
    mutate(
      id = df_raw$id,
      cluster = 
        clustering$classification |> 
        case_match(1 ~ "B", 2 ~ "A", 3 ~ "C")
    ) |> 
    rename(spatial_span = span_spatial)
  
  df_new <-
    left_join(df_raw, new_cols, by = "id") |> 
    unite("subcluster", cluster, group, remove = FALSE) |> 
    mutate(
      cluster = factor(
        cluster,
        levels = c("A", "B", "C"),
        labels = c("A (Control)", "B (Control + Aphant.)", "C (Aphant.)")
      ),
      subcluster = factor(
        subcluster,
        levels = c("A_Control", "B_Control", "B_Aphantasic", "C_Aphantasic"),
        labels = c("A (Control)", "B-Control", "B-Aphant.", "C (Aphant.)")
      )
    ) |> 
    select(id, group, cluster, subcluster, age, everything())
  
  return(df_new)
}