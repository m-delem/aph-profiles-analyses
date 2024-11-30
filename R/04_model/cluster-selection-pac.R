# if (!requireNamespace("pacman")) install.packages("pacman")
pacman::p_load(dplyr, tidyr)

scale_pac <- function(method, clustering) {
  pac_scaled <- 
    clustering$indices$pac |> 
    mutate(across(!k, ~as.numeric(scale(.)))) |> 
    pivot_longer(cols = !k, names_to = "algorithm", values_to = "PAC_scaled") |> 
    mutate(method = method, mean = mean(PAC_scaled), .by = "k") |> 
    relocate(method) |> 
    arrange(mean)
  
  return(pac_scaled)
}

get_k <- function(method, clustering) {
  optimal_k <- 
    scale_pac(method, clustering) |> 
    slice(1) |> 
    pull(k) |> 
    as.numeric()
  
  return(optimal_k)
}

cluster_selection <- function(method, clustering) {
  optimal_k <- get_k(method, clustering)
  
  selection <- 
    clustering$clusters |>
    as_tibble() |>
    select(contains(paste0("k=", optimal_k))) |>
    rename_with(~str_remove(., " k=\\d+"), everything())
  
  return(selection)
}

cluster_summary <- function(df, selection, method, ...){
  df_summary <- 
    selection |> 
    as_tibble() |> 
    mutate(group = df$group) |> 
    pivot_longer(cols = -group, names_to = "consensus", values_to = "cluster") |>
    group_by(consensus, cluster, group) |>
    count() |> 
    mutate(method = method) |> 
    relocate(method) |> 
    arrange(desc(consensus))
  
  return(df_summary)
}
