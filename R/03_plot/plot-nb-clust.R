# if (!requireNamespace("pacman")) install.packages("pacman")
pacman::p_load(
  ggplot2, 
  patchwork,
  see,
  tidyr
)

plot_nb_clust <- function(
    nb_clusters,
    nb_clusters_evolution,
    txt_size_axis  = 22,
    txt_size_label = 7,
    alpha_bars     = .2,  # all bars
    alpha_accent   = .4,  # accentuated bar
    alpha_lines    = .3,  # broken lines
    alpha_ribbon   = .1,  # smooth ribbon
    smoothing      = .5,   # loess smooting
    pal_10_colours = c(
      "#E69F00", "#56B4E9", "#009E73", "#F5C710", "#0072B2", 
      "#D55E00", "#CC79A7", "#6c0009", "#f1afad", "#318a4a"
    )
) {
  # Common styling
  theme_nb_clust <- list(
    scale_y_continuous(
      limits = c(0, 21),
      breaks = breaks_pretty(8),
      expand = c(0, 0),
      oob = squish
    ),
    scale_color_manual(values = pal_10_colours),
    scale_fill_manual(values  = pal_10_colours),
    theme_modern(base_size = 14),
    theme(
      panel.grid.major.y = element_line(),
      panel.grid.minor.y = element_line(),
      axis.title         = element_text(size = txt_size_axis),
      axis.text.x        = element_text(size = txt_size_axis),
      axis.text.y        = element_text(size = txt_size_axis - 6),
      legend.position    = "none"
    )
  )
  
  # Evolution of the number of clusters
  p1 <-
    nb_clusters_evolution |> 
    pivot_longer(
      !n,
      names_to  = "number_of_clusters",
      values_to = "number_of_methods"
    ) |> 
    ggplot(aes(
      x     = n,
      y     = number_of_methods,
      group = number_of_clusters,
      color = number_of_clusters,
      fill  = number_of_clusters
    )) +
    geom_line(alpha = alpha_lines, na.rm = TRUE) +
    geom_smooth(
      formula = y ~ x,  
      method  = "loess",
      alpha   = alpha_ribbon,
      na.rm   = TRUE,
      span    = smoothing
    ) +
    geom_label(
      label = "3 Clusters",
      x     = 90,
      y     = 12,
      color = "#009E73",
      fill  = "white",
      size  = txt_size_label
    ) +
    geom_label(
      label = "2 Clusters",
      x     = 90,
      y     = 7.5,
      color = "#0072B2",
      fill  = "white",
      size  = txt_size_label
    ) +
    scale_x_continuous(
      breaks = c(60, 70, 80, 90, 96),
      labels = c("60", "70", "80", "90", "96"),
      expand = c(0, 0),
      limits = c(60, 96)
    ) +
    labs(
      x = "Sample size",
      y = "Number of clustering indices"
    ) +
    theme_nb_clust
  
  # Final number of clusters
  p2 <-
    ggplot() +
    # all small bars
    geom_bar(
      data  = nb_clusters,
      aes(x = n_Clusters, color = n_Clusters, fill = n_Clusters), 
      alpha = alpha_bars
    ) +
    # 3 clusters accentuated
    geom_bar(
      data      = nb_clusters |> filter(n_Clusters == 3),
      mapping   = aes(x = n_Clusters),
      color     = "#009E73",
      fill      = "#009E73",
      linewidth = 1,
      alpha     = alpha_accent
    ) +
    labs(
      x = "Recommended number of clusters (N = 96)",
      y = NULL
    ) +
    theme_nb_clust
  
  # Joint plot
  p <-
    p1 + p2 + plot_layout(width = c(1, 1))
  
  return(p)
}







