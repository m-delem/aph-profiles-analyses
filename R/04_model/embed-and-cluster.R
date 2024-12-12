# if (!requireNamespace("pacman")) install.packages("pacman")
pacman::p_load(diceR, dimRed, dplyr, tidyr)
# dimRed::installSuggests()

embed_and_cluster <- function(
    df_to_clust, 
    df,
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
      # tSNE = c(list(
      #   d = function(x) stats::dist(x, method = "maximum"), 
      #   theta = 0)),
      # UMAP = c(list(d = "pearson2", knn = 3, method = "naive")),
      # Iso  = c(list(knn = 3)),
      Diff = c(list(
        d = function(x) stats::dist(x, method = "maximum"), 
        ndim = 2)),
      kPCA = c(list(kernel = "laplacedot")),
      MDS  = c(list(d = function(x) stats::dist(x, method = "maximum")))
    )
) {
  # Sub-function 1: Consensus clustering on the embeddings ---------------------
  cluster_vars <- function(
    df, k = 2:4, 
    consensus_f = c(
      "kmodes",
      "CSPA",
      "majority"
      ),
    distance = "maximum",
    ...
  ){
    clustering <-
      df |>
      dice(
        nk       = k,
        k.method = "all",
        p.item   = 0.95,
        reps     = 100,
        algorithms = c(
          "gmm",
          "cmeans",
          "hc",
          "diana",
          "pam",
          "km"
        ),
        hc.method = "complete",
        distance  = distance,
        cons.funs = consensus_f,
        trim      = TRUE,
        reweigh   = FALSE
      )
    
    return(clustering)
  }
  
  # Sub-function 2: Scale and reorder the PAC to identify optimal k ------------
  scale_pac <- function(method, clustering) {
    minmax <- function(x) (x - min(x))/(max(x) - min(x))
    
    pac_scaled <- 
      clustering$indices$pac |> 
      mutate(across(!k, ~as.numeric(scale(.)))) |>
      pivot_longer(
        cols = !k, 
        names_to = "algorithm", 
        values_to = "PAC_scaled"
      ) |> 
      mutate(method = method, mean = mean(PAC_scaled), .by = "k") |> 
      relocate(method) |> 
      arrange(mean)
    
    return(pac_scaled)
  }
  
  # Sub-function 3: Extract optimal k from the PAC table -----------------------
  get_k <- function(method, clustering) {
    optimal_k <- 
      scale_pac(method, clustering) |> 
      slice(1) |> 
      pull(k) |> 
      as.numeric()
    
    return(optimal_k)
  }
  
  # Sub-function 4: k-cluster assignments for each consensus algorithm ---------
  assign_clusters <- function(df, method, clustering) {
    optimal_k <- get_k(method, clustering)
    
    assignments <- 
      clustering$clusters |>
      as_tibble() |>
      select(contains(paste0("k=", optimal_k))) |>
      rename_with(~str_remove(., " k=\\d+"), everything()) |> 
      mutate(id = df$id) |> 
      rename(any_of(c(
        "km"  = "kmodes",
        "maj" = "majority"
      ))) |>
      rename_with(~paste0("cluster_", .), !id) |>
      unnest_longer(!id)
    
    return(assignments)
  }
  
  # Sub-function 5: Rename clusters to 1 = high VVIQ, 2 = mid VVIQ, 3 = low VVIQ
  # ----------------------------------------------------------------------------
  # This restricts the renaming to three clusters because we "know" from tests
  # that three is the optimal solution, but we'll keep the initial assignments
  # in the macro-tibble anyway, just in case.
  rename_clusters <- function(cluster_col, vviq_col) {
    mini_df <- tibble(vviq = vviq_col, cluster = cluster_col)
    
    renamed_clusters <-
      mini_df |> 
      mutate(mean = mean(vviq), .by = cluster) |> 
      mutate(
        cluster = case_when(
          mean == max(mean) ~ 1,
          mean == min(mean) ~ 3,
          TRUE ~ 2
        )
      ) |> 
      pull(cluster)
    
    return(renamed_clusters)
  }
  # Apply this function to the assignments df
  rename_assigns <- function(df, assignments) {
    renamed_assignments <- 
      assignments |> 
      mutate(
        across(contains("cluster"), ~ rename_clusters(., df$vviq))
      )
    
    return(renamed_assignments)
  }

  # Sub-function X: Summary of the embeddings and consensus clusterings --------
  cluster_summary <- function(df, assignments, method, ...){
    df_summary <- 
      assignments |> 
      mutate(group = df$group) |> 
      pivot_longer(
        cols = !c(id, group), 
        names_to = "consensus", 
        values_to = "cluster"
      ) |>
      group_by(consensus, cluster, group) |>
      count() |> 
      mutate(method = method) |> 
      relocate(method) |> 
      arrange(desc(consensus))
    
    return(df_summary)
  }
  
  # Main computation: all the results above gathered in a tibble ---------------
  results <- 
    tibble(
      # df          = list(df),
      # df_to_clust = list(df_to_clust),
      method      = embed_methods,
      params      = params
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
      # Dimensionality reduction quality metrics
      q_local    = quality(dim_reds, "Q_local"),
      q_global   = quality(dim_reds, "Q_global"),
      q_R_NX     = quality(dim_reds, "mean_R_NX"),
      q_cophen   = quality(dim_reds, "cophenetic_correlation"),
      q_distance = quality(dim_reds, "distance_correlation"),
      clustering = list(cluster_vars(embeddings, k = 2:4)),
      pac_scaled = list(scale_pac(method, clustering)),
      optimal_k  = list(get_k(method, clustering)),
      assignment = list(assign_clusters(df, method, clustering)),
      renamed_clusters = list(rename_assigns(df, assignment)),
      # clusters_summary = list(cluster_summary(df, assignments, method))
    ) |> 
    ungroup()
  
  return(results)
}

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
      # scaled_choice  = most_freq("scaled"),
      cluster = case_when(
        A_count > B_count & A_count > C_count ~ "A",
        B_count > A_count & B_count > C_count ~ "B",
        C_count > A_count & C_count > B_count ~ "C",
        B_count == C_count ~ "B",
        A_count == B_count ~ "A",
        all(sapply(list(A_count, B_count), function(x) x == C_count)) ~ "B",
        TRUE               ~ "B"
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

