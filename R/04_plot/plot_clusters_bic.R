pacman::p_load(BiocManager)

plot_clusters_bic <- function(
    mclust_object,
    txt_big  = 7,
    txt_mid  = 6,
    txt_smol = 5,
    size = 0.2
    ) {
  p <- 
    factoextra::fviz_mclust_bic(
      mclust_object, 
      shape   = "model",
      size    = size,
      palette = colorRampPalette(see::okabeito_colors())(14)
    ) + 
    labs(
      title  = NULL,
      colour = "Model type",
      shape  = "Model type"
    ) +
    theme(
      plot.subtitle         = element_text(size = txt_big),
      plot.margin           = margin(1, 1, 0, 1, "mm"),
      legend.position       = "bottom",
      legend.margin         = margin(0, 0, 1, 0, "mm"),
      legend.box.spacing    = unit(0, "mm"),
      legend.title.position = "top",
      legend.title          = element_text(size = txt_big),
      legend.text           = element_text(size = txt_mid),
      axis.title            = element_text(size = txt_big),
      axis.text             = element_text(size = txt_smol),
      axis.line             = element_line(linewidth = size),
      axis.ticks            = element_line(linewidth = size),
    )
  
  # reorder the layers to put the red line in the background
  p$layers <- p$layers[c(3, 1, 2)]
  
  return(p)
}