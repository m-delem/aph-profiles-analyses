
# Get the majority cluster assignment for each subject across all clusterings
align_clusters <- function(results_tibble, df) {
  df_clustered <- 
    cluster_results |>
    select(
      name, 
      method, 
      clusters
    ) |> 
    unnest(!c(name, method)) |> 
    pivot_wider(
      names_from = c(name, method), 
      values_from = c(contains("cluster"))
    ) |> 
    left_join(df, by = "id") |> 
    rowwise() |>
    mutate(
      A_score = sum(c_across(contains("cluster")) == 1),
      B_score = sum(c_across(contains("cluster")) == 2),
      C_score = sum(c_across(contains("cluster")) == 3),
      cluster = case_when(
        C_score > A_score & C_score > B_score ~ "C",
        B_score > A_score & B_score > C_score ~ "B",
        A_score >= B_score & vviq <= 32 ~ "B",   # Aph. misassignment
        # A_score >= B_score & vviq >= 58 ~ "A", # mean of the Control group
        A_score > B_score & A_score > C_score ~ "A",
        # B_score == C_score ~ "B",
        TRUE ~ "B"
      )
    ) |> 
    ungroup() |>   
    select(!c(contains("_score"), contains("cluster_"))) |>
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
    select(id, group, cluster, subcluster, everything())
  
  return(df_clustered)
}