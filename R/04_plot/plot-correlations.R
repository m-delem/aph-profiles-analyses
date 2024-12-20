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
    shape       = 21,
    stroke      = 0.1,
    axis_text   = 6,
    matrix_text = 5,
    node_size   = 14,
    node_text_size  = 5,
    label_text_size = 2
) {
  # Matrix ---------------------------------------------------------------------
  correlation_matrix <-
    correlations |> 
    mutate(r = if_else(abs(r) < 0.01, 0, r)) |>
    summary(digits = 2) |>
    visualisation_recipe(
      show_data  = "tiles", 
      tile       = list(colour = "black", linewidth = 0.05),
      text       = list(size = matrix_text, size.unit = "pt"),
      scale_fill = list(
        high = "#009e73", 
        low  = "firebrick2", 
        name = "r"
      )
    ) |> 
    plot() +
    scale_x_discrete(position = "top") +
    scale_y_discrete(position = "left") +
    labs(title = NULL) + 
    theme_modern() + 
    theme(
      legend.position = "none",
      axis.text.x     = element_text(size = axis_text, angle = 45, hjust = 0),
      axis.text.y     = element_text(size = axis_text),
      axis.line       = element_blank()
    )
  
  # Graph ----------------------------------------------------------------------
  correlation_graph <-
    correlations |>
    ggraph(
      layout   = "linear",
      circular = TRUE
    ) +
    geom_edge_arc(
      strength = 0.2,
      mapping  = aes(
        label       = round(r, 2),
        filter      = (p < 0.05),
        edge_colour = r,
        edge_width  = r
      ),
      label_size    = unit(label_text_size, "pt"),
      check_overlap = TRUE
    ) +
    # Base black nodes ---------------------------------------------------------
    geom_node_point(
      shape  = shape, 
      size   = node_size, 
      stroke = 0, 
      fill   = "black"
    ) +
    # Coloured nodes -----------------------------------------------------------
    # Psi-Q's in dark blue
    geom_node_point(
      aes(
        filter = (str_detect(name, "Psi") & !str_detect(name, "Audition"))
      ),
      shape  = shape,
      size   = node_size,
      fill   = "#0072B2",
      stroke = stroke,
      colour = "black"
    ) +
    # Visual imagery in blue
    geom_node_point(
      aes(filter = str_detect(name, "VVIQ|OSIVQ\nObject|Psi-Q\nVisual")),
      shape  = shape,
      size   = node_size,
      fill   = "#56B4E9",
      stroke = stroke,
      colour = "black"
    ) +
    # Spatial imagery in light orange
    geom_node_point(
      aes(filter = str_detect(name, "SRI|OSIVQ\nSpatial")),
      shape  = shape,
      size   = node_size,
      fill   = "#E69F00",
      stroke = stroke,
      colour = "black"
    ) +
    # Verbal strategies in green
    geom_node_point(
      aes(filter = str_detect(name, "OSIVQ\nVerbal")),
      shape  = shape,
      size   = node_size,
      fill   = "#009E73",
      stroke = stroke,
      colour = "black"
    ) +
    # Raven + Digit in pink
    geom_node_point(
      aes(filter = str_detect(name, "Digit\nspan|Raven\nMatrices")),
      shape  = shape,
      size   = node_size,
      fill   = "#CC79A7",
      stroke = stroke,
      colour = "black"
    ) +
    # Spatial span in yellow
    geom_node_point(
      aes(filter = str_detect(name, "Spatial\nspan")),
      shape  = shape,
      size   = node_size,
      fill   = "#999999",
      stroke = stroke,
      colour = "black"
    ) +
    # Verbal reasoning in dark orange
    geom_node_point(
      aes(filter = str_detect(name, "Similarities")),
      shape  = shape,
      size   = node_size,
      fill   = "#D55E00",
      stroke = stroke,
      colour = "black"
    ) +
    # End of coloured nodes ----------------------------------------------------
    geom_node_text(
      mapping   = aes(label = name), 
      colour    = "white", 
      fontface  = "bold",
      size      = node_text_size,
      size.unit = "pt"
    ) +
    scale_edge_colour_gradient2(
      limits = c(-1, 1),
      low    = "firebrick2",
      mid    = "white",
      high   = "#009e73",
      breaks = breaks_pretty(8)
    ) +
    scale_edge_width(range = c(2, 3)) +
    coord_fixed(clip = "off") +
    guides(edge_width = "none") +
    labs(title = NULL) +
    theme(
      panel.background = element_rect(fill = "transparent"),
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
    plot_layout(design = layout) &
    theme(
      plot.margin = margin(0, 1, 0, 0, "mm")
      )
  
  return(correlation_joint)
}