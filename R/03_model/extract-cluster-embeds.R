# if (!requireNamespace("pacman")) install.packages("pacman")
pacman::p_load(dplyr, tidyr)

extract_cluster_embeds <- function(reduction, cluster_results, df) {
  df_embeds <- 
    cluster_results[[reduction]] |> 
    rowwise() |> 
    mutate(embeddings = list(
      embeddings |> 
        rename(dim_1 = 1, dim_2 = 2) |> 
        bind_cols(selection |> select(majority)) |> 
        bind_cols(df |> select(vviq)) |> 
        mutate(
          cluster = rename_clusters(majority, vviq),
          cluster = paste0("Cluster ", cluster)
        )
    ) 
    ) |> 
    select(projection = method, embeddings) |> 
    unnest(embeddings) |> 
    mutate(reduction = reduction)
  
  return(df_embeds)
}
