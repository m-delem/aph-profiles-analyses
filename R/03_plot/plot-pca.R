# if (!requireNamespace("pacman")) install.packages("pacman")
pacman::p_load(factoextra, ggplot2, see)

plot_pca <- function(
    clustering,
    palette = c("#56B4E9", "#E69F00", "#009E73"),
    txt_size_axis   = 18,
    txt_size_legend = 22,
    ellipse_level   = .7,
    ellipse_alpha   = .1
) {
  # Clustering PCA projection
  cluster_pca <-
    clustering |> 
    fviz_mclust(
      geom = "text",
      ellipse.type = "norm",
      ellipse.level = ellipse_level,
      ellipse.alpha = ellipse_alpha,
      shape = 16,
      repel = TRUE
    ) +
    scale_color_manual(values = palette, guide = "none") +
    scale_fill_manual(values  = palette, guide = "none") +
    theme_modern() +
    theme(
      axis.title = element_text(size = txt_size_axis),
      axis.text  = element_text(size = txt_size_axis - 2),
      plot.margin = margin(0,0,0,0)
    ) +
    labs(
      title = NULL,
      subtitle = NULL,
      x = "Dimension 1 (35.5%)",
      y = "Dimension 2 (19.8%)"
    )
  
  return(cluster_pca)
}
  



