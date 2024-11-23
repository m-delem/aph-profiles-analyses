# if (!requireNamespace("pacman")) install.packages("pacman")
pacman::p_load(dplyr)
source(here("R/02_wrangle/reduce-vars.R"))

#' Add the clustering and reduced variables to the main data frame
#'
#' @param df         The main data frame
#' @param clustering The clustering object
#'
#' @return A data frame with the clustering classification and reduced variables
#' 
add_cluster_vars <- function(df, clustering) {
  withr::local_options(list(warn = -1))
  
  new_cols <-
    df |> 
    reduce_vars() |> 
    mutate(
      id = df$id,
      cluster = clustering$classification |> 
        case_match(1 ~ "Cluster B", 2 ~ "Cluster A", 3 ~ "Cluster C") |> 
        factor(levels = c("Cluster A", "Cluster B", "Cluster C"))
    ) |> 
    rename(spatial_span = span_spatial)
  
  df_new <-
    left_join(df, new_cols, by = "id") |> 
    mutate(
      subcluster = case_when(
        cluster == "Cluster A"                         ~ "A (Control)",
        cluster == "Cluster B" & group == "Control"    ~ "B-Control",
        cluster == "Cluster B" & group == "Aphantasic" ~ "B-Aph.",
        cluster == "Cluster C"                         ~ "C (Aph.)",
      ) |> factor(levels = c("A (Control)", "B-Control", "B-Aph.", "C (Aph.)"))
    ) |> 
    select(id, group, cluster, subcluster, age, everything())
  
  return(df_new)
}
