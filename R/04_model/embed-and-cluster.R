# if (!requireNamespace("pacman")) install.packages("pacman")
pacman::p_load(dimRed, dplyr)
# dimRed::installSuggests()

embed_and_cluster <- function(
    df,
    df_to_clust, 
    embed_methods = c( # see dimRedMethodList()
      # "tSNE",
      # "UMAP",
      # "Isomap",
      "DiffusionMaps",
      "kPCA",
      "MDS"
    ),
    # additional params for each method
    params = list(
      # tSNE = c(list(d = function(x) stats::dist(x, method = "maximum"), theta = 0)),
      # UMAP = c(list(d = "pearson2", knn = 3, method = "naive")),
      # Iso  = c(list(knn = 3)),
      Diff = c(list(d = function(x) stats::dist(x, method = "maximum"), ndim = 2)),
      kPCA = c(list(kernel = "laplacedot")),
      MDS  = c(list(d = function(x) stats::dist(x, method = "maximum")))
    )
) {
  results <- 
    tibble(
      method = embed_methods,
      params = params
    ) |> 
    rowwise() |> 
    mutate(
      dim_reds = list(do.call(
        dimRed::embed, 
        modifyList(c(list(.data = df_to_clust, .method = method)), params))),
      embeddings = list(
        dim_reds |> 
          getDimRedData() |> 
          dimRed::as.data.frame() |> 
          mutate(across(everything(), ~as.numeric(scale(.))))
      ),
      q_local    = quality(dim_reds, "Q_local"),
      q_global   = quality(dim_reds, "Q_global"),
      q_R_NX     = quality(dim_reds, "mean_R_NX"),
      q_cophen   = quality(dim_reds, "cophenetic_correlation"),
      q_distance = quality(dim_reds, "distance_correlation")
    ) |> 
    mutate(
      clustering = list(cluster_vars(embeddings, k = 2:4)),
      pac_scaled = list(scale_pac(method, clustering)),
      optimal_k  = list(get_k(method, clustering)),
      selection  = list(cluster_selection(method, clustering)),
      summary    = list(cluster_summary(df, selection, method)),
      id         = list(df$id)
    ) |> 
    ungroup()
  
  return(results)
}