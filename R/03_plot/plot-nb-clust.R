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
    txt_size_axis  = 24,
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
  # Final number of clusters
  nb_clus_plot <-
    ggplot() +
    geom_bar(
      data = nb_clusters,
      aes(x = n_Clusters, color = n_Clusters, fill = n_Clusters), 
      alpha = alpha_bars
    ) +
    geom_bar(
      data = nb_clusters |> filter(n_Clusters == 3),
      aes(x = n_Clusters),
      color = "#009E73",
      fill = "#009E73",
      linewidth = 1,
      alpha = alpha_accent
    ) +
    scale_y_continuous(
      limits = c(0, 18),
      breaks = seq(1, 17, 1),
      labels = c(1,"","","", 5,"","","","", 10,"","","","", 15, "", ""),
      expand = c(0, 0),
      oob = squish
    ) +
    scale_color_manual(values = pal_10_colours) +
    scale_fill_manual(values  = pal_10_colours) +
    theme_modern(base_size = 14) +
    theme(
      panel.grid.major.y = element_line(),
      axis.title = element_text(size = txt_size_axis),
      axis.text  = element_text(size = txt_size_axis),
      legend.position = "none"
    ) +
    labs(
      x = "Recommended number of clusters (N = 96)",
      y = NULL
    )
  
  # Evolution of the number of clusters
  nb_clus_plot_evo <-
    # Data
    nb_clusters_evolution |> 
    pivot_longer(
      !n,
      names_to = "number_of_clusters",
      values_to = "number_of_methods"
    ) |> 
    ggplot(aes(
      x = n,
      y = number_of_methods,
      group = number_of_clusters,
      color = number_of_clusters,
      fill  = number_of_clusters
    )) +
    
    # Geoms
    geom_line(alpha = alpha_lines, na.rm = TRUE) +
    geom_smooth(
      formula = y ~ x,  
      method = "loess",
      alpha = alpha_ribbon,
      na.rm = TRUE,
      span = smoothing
    ) +
    geom_label(
      label = "3 Clusters",
      x = 79,
      y = 14,
      color = "#009E73",
      fill = "white",
      size = txt_size_label
    ) +
    geom_label(
      label = "2 Clusters",
      x = 79,
      y = 9,
      color = "#0072B2",
      fill = "white",
      size = txt_size_label
    ) +
    
    # Scales
    scale_x_continuous(
      breaks = c(60, 70, 80, 90, 96),
      labels = c("60", "70", "80", "90", "96"),
      expand = c(0, 0),
      limits = c(60, 96)
    ) +
    scale_y_continuous(
      limits = c(0, 18),
      breaks = seq(1,17,1),
      labels = c(1,"","","", 5,"","","","", 10,"","","","", 15, "", ""),
      expand = c(0, 0),
      oob = squish
    ) +
    scale_color_manual(
      name = "Recommended\nnumber of\nclusters", values = pal_10_colours) +
    scale_fill_manual(
      name = "Recommended\nnumber of\nclusters", values = pal_10_colours) +
    
    theme_modern(base_size = 14) +
    # Theme
    theme(
      panel.grid.major.y = element_line(),
      axis.title = element_text(size = txt_size_axis),
      axis.text  = element_text(size = txt_size_axis),
      legend.position = "none"
    ) +
    labs(
      x = "Sample size",
      y = "Number of clustering indices"
    )
  
  # Joint plot
  nb_clus_plot_joint <-
    nb_clus_plot_evo + nb_clus_plot + 
    plot_layout(width = c(1, 1))
  
  return(nb_clus_plot_joint)
}







