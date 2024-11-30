# if (!requireNamespace("pacman")) install.packages("pacman")
pacman::p_load(dplyr, tidyr)

get_long_clusters <- function(df) {
  df_long <-
    df |> 
    pivot_longer(
      cols = contains("cluster"),
      names_to  = "Method",
      values_to = "Cluster"
    ) |> 
    mutate(
      Method =
        case_match(
          Method,
          "cluster_DM"  ~ "Diffusion Maps",
          "cluster_Iso" ~ "Isomap",
          .default = Method
        ) |> 
        str_remove("cluster_")
    )
  
  return(df_long)
}