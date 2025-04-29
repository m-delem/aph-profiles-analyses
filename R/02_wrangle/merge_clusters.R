# Add the clustering and reduced variables to the main data frame
merge_clusters <- function(df_raw, df_red, clustering) {
  withr::local_options(list(warn = -1))
  
  new_cols <-
    df_red |>
    dplyr::mutate(
      id = df_raw$id,
      cluster = 
        clustering$classification |> 
        dplyr::case_match(1 ~ "B", 2 ~ "C", 3 ~ "A")
    ) |> 
    dplyr::rename(spatial_span = span_spatial)
  
  df_new <-
    dplyr::left_join(df_raw, new_cols, by = "id") |> 
    tidyr::unite("subcluster", cluster, group, remove = FALSE) |> 
    dplyr::mutate(
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
    dplyr::select(id, group, cluster, subcluster, age, tidyselect::everything())
  
  return(df_new)
}