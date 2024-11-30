# if (!requireNamespace("pacman")) install.packages("pacman")
pacman::p_load(dplyr, ggplot2, see)

plot_clusters_embedded <- function(
    method, embeddings, selection, 
    consensus_function  = "majority", 
    ...
) {
  x <- sym(names(embeddings)[1])
  y <- sym(names(embeddings)[2])
  
  p <- 
    embeddings |> 
    as_tibble() |> 
    bind_cols(selection[, consensus_function]) |>
    rename(cluster = {{ consensus_function }}) |>
    ggplot(aes({{ x }}, {{ y }}, color = factor(cluster))) +
    geom_point(size = 3) +
    scale_colour_okabeito(name = "Cluster") +
    labs(
      title = paste0(method, " embedding, ", consensus_function, " consensus")
    ) +
    theme_modern()
  
  print(p)
}
