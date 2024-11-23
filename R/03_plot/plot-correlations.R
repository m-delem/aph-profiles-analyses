# if (!requireNamespace("pacman")) install.packages("pacman")
pacman::p_load(
  correlation,
  dplyr, 
  here,
  ggplot2, 
  ggraph,
  patchwork,
  see
)

# Plot a correlation matrix and a circular correlation graph
plot_correlations <- function(
    correlations, 
    shape = 21,
    stroke = 1
) {
  # Matrix ------------------------------
  correlation_matrix <-
    correlations |> 
    summary() |>
    visualisation_recipe(
      show_data = "tiles", 
      tile = list(colour = "black"),
      # show_data = "points",
      # scale = list(range = c(10, 20)),
      scale_fill = list(
        high = "#009e73", 
        low  = "firebrick2", 
        name = "r"
      )
    ) |> 
    plot() +
    # coord_fixed() +
    scale_x_discrete(
      position = "top", 
      # limits = rev
    ) +
    scale_y_discrete(
      position = "left",
    ) +
    labs(title = NULL) + 
    theme_modern() + 
    theme(
      legend.position = "none",
      axis.text.x     = element_text(angle = 45, hjust = 0),
      axis.line       = element_blank(),
      plot.margin     = margin(0, 0, 0, 0, "cm")
    )
  
  # Graph ------------------------------
  correlation_graph <-
    correlations |>
    ggraph(
      layout = "linear",
      circular = TRUE
    ) +
    geom_edge_arc(
      strength = 0.2,
      aes(
        label = round(r, 2),
        filter = (p < 0.05),
        edge_colour = r,
        edge_width  = r
      )
    ) +
    geom_node_point(size = 25) +
    geom_node_point(
      aes(filter = (str_detect(name, "Psi") & !str_detect(name, "Audition"))),
      size = 25,
      shape = shape,
      # fill = "black",
      fill = "#D55E00",
      stroke = stroke,
      # colour = "#D55E00"
      colour = "black"
    ) +
    geom_node_point(
      aes(filter = str_detect(name, "VVIQ|OSIVQ\nObject|Psi-Q\nVisual")),
      size = 25,
      shape = shape,
      # fill = "black",
      fill = "#56B4E9",
      stroke = stroke,
      # colour = "#56B4E9"
      colour = "black"
    ) +
    geom_node_point(
      aes(filter = str_detect(name, "SRI|OSIVQ\nSpatial")),
      size = 25,
      shape = shape,
      # fill = "black",
      fill = "#E69F00",
      stroke = stroke,
      # colour = "#E69F00",
      colour = "black"
    ) +
    geom_node_point(
      aes(filter = str_detect(name, "Digit\nspan|Raven\nMatrices")),
      size = 25,
      shape = shape,
      # fill = "black",
      fill = "#009E73",
      stroke = stroke,
      # colour = "#009E73"
      colour = "black"
    ) +
    geom_node_text(
      aes(label = name), 
      colour = "white", 
      fontface = "bold",
      size = 3.3
    ) +
    scale_edge_colour_gradient2(
      limits = c(-1, 1),
      low    = "firebrick2",
      mid    = "white",
      high   = "#009e73",
      breaks = breaks_pretty(8)
    ) +
    scale_edge_width(range = c(4, 8)) +
    guides(edge_width = "none") +
    labs(title = NULL) +
    theme(
      panel.background = element_rect(fill = "transparent"),
      plot.margin      = margin(0, 0, 0, 0.5, "cm"),
      legend.position  = "none"
    )
  
  # Patchwork manual layout -----------------------
  layout <- c(
    area(t = 1, l = 1, b = 5, r = 3),
    area(t = 2, l = 3, b = 5, r = 4)
  )
  
  # Final figure
  correlation_joint <-
    (correlation_matrix + correlation_graph) +
    plot_layout(design = layout)
  
  return(correlation_joint)
}
