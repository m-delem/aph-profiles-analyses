# if (!requireNamespace("pacman")) install.packages("pacman")
pacman::p_load(BiocManager, factoextra, see)

plot_clusters_bic <- function(mclust_object) {
  p <- 
    fviz_mclust_bic(
      mclust_object, 
      shape   = "model",
      size    = 0.2,
      palette = colorRampPalette(see::okabeito_colors())(14)
    ) + 
    labs(
      title  = NULL,
      colour = "Model type",
      shape  = "Model type"
    ) +
    theme(
      plot.subtitle         = element_text(size = 7),
      plot.margin           = margin(1, 1, 0, 1, "mm"),
      legend.position       = "bottom",
      legend.margin         = margin(0, 0, 1, 0, "mm"),
      legend.box.spacing    = unit(0, "mm"),
      legend.title.position = "top",
      legend.title          = element_text(size = 7),
      legend.text           = element_text(size = 6),
      axis.title            = element_text(size = 7),
      axis.text             = element_text(size = 5),
      axis.line             = element_line(size = 0.2),
      axis.ticks            = element_line(size = 0.2),
    )
  
  # reorder the layers to put the red line in the background
  p$layers <- p$layers[c(3, 1, 2)]
  
  return(p)
}