
# Get the majority cluster assignment for each subject across all clusterings
align_clusters <- function(df) {
  most_freq <- function(x) {
    m <- names(which.max(table(c_across(contains(x)))))
    if (is.null(m)) return(NA)
    
    return(m)
  }
  
  df |> 
    rowwise() |>
    mutate(
      A_count = sum(c_across(contains("cluster")) == "A"),
      B_count = sum(c_across(contains("cluster")) == "B"),
      C_count = sum(c_across(contains("cluster")) == "C"),
      scaled_choice  = most_freq("scaled"),
      cluster = case_when(
        A_count > B_count & A_count > C_count ~ "A",
        B_count > A_count & B_count > C_count ~ "B",
        C_count > A_count & C_count > B_count ~ "C",
        B_count == C_count ~ "B",
        A_count == B_count ~ scaled_choice,
        TRUE               ~ scaled_choice
      )
    ) |>   
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
    # select(!c(contains("count"), contains("cluster_"))) |>
    select(id, group, cluster, subcluster, everything()) |> 
    ungroup()
}