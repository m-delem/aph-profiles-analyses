# if (!requireNamespace("pacman")) install.packages("pacman")
pacman::p_load(ggplot2, superb)

plot_clusters_radar <- function(
    data, 
    formula,
    palette,
    txt_big   = 5,
    txt_mid   = 4,
    txt_smol  = 2.5,
    # dot_big   = 0.3,
    dot_smol  = 0.1,
    # lw_big    = 0.3,
    lw_smol   = 0.1,
    y_off     = 20,
    h_off     = 2,
    v_off     = 0,
    key       = 2,
    ...
) {
  p <- 
    superb(
      # formula        = value ~ Variable + Subcluster, 
      formula        = formula,
      data           = data,
      plotStyle      = "circularline",
      pointParams    = list(size = dot_smol),
      lineParams     = list(linewidth = lw_smol),
      errorbarParams = list(linewidth = lw_smol, show.legend = FALSE),
      adjustments    = list(purpose = "single")
      # factorOrder    = c("Variable", "Subcluster")
    ) + 
    scale_colour_manual(values = palette) +
    scale_fill_manual(values   = palette) +
    scale_y_continuous(
      limits = c(0, 1),
      breaks = breaks_pretty(),
      expand = expansion(c(0, 0.02))
    ) +
    theme_minimal(base_size = txt_mid) +
    theme(
      # plot wise elements
      plot.title       = element_text(size = txt_big),
      panel.grid.minor = element_blank(),
      plot.margin      = margin(0, h_off, v_off, h_off, "mm"),
      # legend
      legend.position  = "top",
      legend.title     = element_blank(),
      legend.text      = element_text(size = txt_mid),
      legend.key.size  = unit(key, "mm"),
      # y axis
      axis.text.y      = element_text(
        size   = txt_smol, 
        margin = margin(0, -y_off, 0, 0, "mm"),
        hjust  = 0, 
        vjust  = 0.5
      ),
      axis.line        = element_blank(),
      axis.title.y     = element_blank(),
      # x axis
      axis.text.x      = element_text(size = txt_mid),
      axis.title.x     = element_blank(),
    )
  
  return(p)
}