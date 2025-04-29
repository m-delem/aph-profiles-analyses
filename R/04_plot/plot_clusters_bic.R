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
    ggplot2::labs(
      title  = NULL,
      colour = "Model type",
      shape  = "Model type"
    ) +
    ggplot2::theme(
      plot.subtitle         = ggplot2::element_text(size = txt_big),
      plot.margin           = ggplot2::margin(1, 1, 0, 1, "mm"),
      legend.position       = "bottom",
      legend.margin         = ggplot2::margin(0, 0, 1, 0, "mm"),
      legend.box.spacing    = grid::unit(0, "mm"),
      legend.title.position = "top",
      legend.title          = ggplot2::element_text(size = txt_big),
      legend.text           = ggplot2::element_text(size = txt_mid),
      axis.title            = ggplot2::element_text(size = txt_big),
      axis.text             = ggplot2::element_text(size = txt_smol),
      axis.line             = ggplot2::element_line(linewidth = size),
      axis.ticks            = ggplot2::element_line(linewidth = size),
    )
  
  # reorder the layers to put the red line in the background
  p$layers <- p$layers[c(3, 1, 2)]
  
  return(p)
}