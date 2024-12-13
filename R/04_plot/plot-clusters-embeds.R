# if (!requireNamespace("pacman")) install.packages("pacman")
pacman::p_load(dplyr, ggplot2, see)

plot_clusters_embeds <- function(cluster_results, df) {
  proj_names <- c(
    DiffusionMaps = "Diffusion Maps",
    kPCA = "Kernel PCA",
    MDS = "MDS"
  )
  reds_names <- c(
    scaled = "After\nvariable\nscaling",
    reduced = "After\npartial\ncorrelation\nreduction"
  )
  
  p <- 
    purrr::map(c("scaled", "reduced"), ~{
      extract_cluster_embeds(., cluster_results, df)
    }) |> 
    bind_rows() |> 
    mutate(reduction = factor(reduction, levels = c("scaled", "reduced"))) |>
    ggplot(aes(x = dim_1, y = dim_2, colour = factor(cluster))) +
    geom_jitter(size = 0.5, width = 0) +
    facet_grid(
      rows = vars(reduction), 
      cols = vars(projection),
      scales = "free",
      switch = "y",
      labeller = labeller(
        reduction  = reds_names,
        projection = proj_names
      )
    ) +
    labs(
      x = "Dimension 1",
      y = "Dimension 2"
    ) +
    scale_x_continuous(
      expand = expansion(c(0.05, 0.05)),
      breaks = breaks_pretty()
    ) +
    scale_y_continuous(
      expand = expansion(c(0.05, 0.05)),
      breaks = breaks_pretty(),
      position = "right"
    ) +
    scale_colour_manual(values = c("#56B4E9", "#E69F00", "#009E73")) +
    scale_fill_manual(values = c("#56B4E9", "#E69F00", "#009E73")) +
    theme_bw(base_size = 5) +
    theme(
      legend.position      = "top",
      legend.title         = element_blank(),
      legend.text          = element_text(size = 7), 
      legend.key.size      = unit(1, "mm"),
      legend.key.spacing.x = unit(5, "mm"),
      axis.title           = element_text(size = 6.5),
      axis.title.y.right   = element_text(margin = margin(0, 0, 0, 2, "mm")),
      axis.title.x         = element_text(margin = margin(2, 0, 0, 0, "mm")),
      axis.ticks           = element_line(color = "grey80", linewidth = 0.05),
      axis.line            = element_blank(),
      strip.text           = element_text(size = 7, 
                                          margin = margin(1.5, 0, 1.5, 0, "mm")),
      strip.text.y.left    = element_text(angle = 0,
                                          margin = margin(0, 1, 0, 1, "mm")),
      strip.background     = element_rect(linewidth = 0.1,
                                          color = "grey80", fill = "grey95"),
      panel.border         = element_rect(linewidth = 0.1,
                                          color = "grey80", fill = NA),
      panel.spacing.x      = unit(0, "mm"),
      panel.spacing.y      = unit(0, "mm"),
      panel.grid.major     = element_line(color = "grey95", linewidth = 0.05),
      panel.grid.minor     = element_blank()
    )
  
  return(p)
}
