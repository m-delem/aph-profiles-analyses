pacman::p_load(ggraph, patchwork)

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
    dplyr::mutate(r = ifelse(abs(r) < 0.01, 0, r)) |>
    summary(digits = 2) |>
    correlation::visualisation_recipe(
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
    ggplot2::scale_x_discrete(position = "top") +
    ggplot2::scale_y_discrete(position = "left") +
    ggplot2::labs(title = NULL) + 
    see::theme_modern() + 
    ggplot2::theme(
      legend.position = "none",
      axis.text.x     = ggplot2::element_text(
        size = axis_text, 
        angle = 45, 
        hjust = 0
      ),
      axis.text.y = ggplot2::element_text(size = axis_text),
      axis.line   = ggplot2::element_blank()
    )
  
  # Graph ----------------------------------------------------------------------
  correlation_graph <-
    correlations |>
    ggraph::ggraph(
      layout   = "linear",
      circular = TRUE
    ) +
    ggraph::geom_edge_arc(
      strength = 0.2,
      mapping  = ggplot2::aes(
        label       = round(r, 2),
        filter      = (p < 0.05),
        edge_colour = r,
        edge_width  = r
      ),
      label_size    = grid::unit(label_text_size, "pt"),
      check_overlap = TRUE
    ) +
    # Base black nodes ---------------------------------------------------------
    ggraph::geom_node_point(
      shape  = shape, 
      size   = node_size, 
      stroke = 0, 
      fill   = "black"
    ) +
    # Coloured nodes -----------------------------------------------------------
    # Psi-Q's in dark blue
    ggraph::geom_node_point(
      mapping = ggplot2::aes(
        filter = (
          stringr::str_detect(name, "Psi") & 
            !stringr::str_detect(name, "Audition"))
      ),
      shape  = shape,
      size   = node_size,
      fill   = "#0072B2",
      stroke = stroke,
      colour = "black"
    ) +
    # Visual imagery in blue
    ggraph::geom_node_point(
      mapping = ggplot2::aes(
        filter = stringr::str_detect(name, "VVIQ|OSIVQ\nObject|Psi-Q\nVisual")
      ),
      shape  = shape,
      size   = node_size,
      fill   = "#56B4E9",
      stroke = stroke,
      colour = "black"
    ) +
    # Spatial imagery in light orange
    ggraph::geom_node_point(
      mapping = ggplot2::aes(
        filter = stringr::str_detect(name, "SRI|OSIVQ\nSpatial")
      ),
      shape  = shape,
      size   = node_size,
      fill   = "#E69F00",
      stroke = stroke,
      colour = "black"
    ) +
    # Verbal strategies in green
    ggraph::geom_node_point(
      mapping = ggplot2::aes(
        filter = stringr::str_detect(name, "OSIVQ\nVerbal")
      ),
      shape  = shape,
      size   = node_size,
      fill   = "#009E73",
      stroke = stroke,
      colour = "black"
    ) +
    # Raven + Digit in pink
    ggraph::geom_node_point(
      mapping = ggplot2::aes(
        filter = stringr::str_detect(name, "Digit\nspan|Raven\nMatrices")
      ),
      shape  = shape,
      size   = node_size,
      fill   = "#CC79A7",
      stroke = stroke,
      colour = "black"
    ) +
    # Spatial span in yellow
    ggraph::geom_node_point(
      mapping = ggplot2::aes(
        filter = stringr::str_detect(name, "Spatial\nspan")
      ),
      shape  = shape,
      size   = node_size,
      fill   = "#999999",
      stroke = stroke,
      colour = "black"
    ) +
    # Verbal reasoning in dark orange
    ggraph::geom_node_point(
      mapping = ggplot2::aes(
        filter = stringr::str_detect(name, "Similarities")
      ),
      shape  = shape,
      size   = node_size,
      fill   = "#D55E00",
      stroke = stroke,
      colour = "black"
    ) +
    # End of coloured nodes ----------------------------------------------------
    ggraph::geom_node_text(
      mapping   = ggplot2::aes(label = name), 
      colour    = "white", 
      fontface  = "bold",
      size      = node_text_size,
      size.unit = "pt"
    ) +
    ggraph::scale_edge_colour_gradient2(
      limits = c(-1, 1),
      low    = "firebrick2",
      mid    = "white",
      high   = "#009e73",
      breaks = scales::breaks_pretty(8)
    ) +
    ggraph::scale_edge_width(range = c(2, 3)) +
    ggplot2::coord_fixed(clip = "off") +
    ggplot2::guides(edge_width = "none", edge_colourbar = "none") +
    ggplot2::labs(title = NULL) +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "transparent"),
      legend.position  = "none"
    )
  
  # Patchwork manual layout -----------------------
  layout <- c(
    patchwork::area(t = 1, l = 1, b = 5, r = 3),
    patchwork::area(t = 2, l = 3, b = 5, r = 4)
  )
  
  # Final figure
  correlation_joint <-
    (correlation_matrix + correlation_graph) +
    patchwork::plot_layout(design = layout) &
    ggplot2::theme(
      plot.margin = ggplot2::margin(0, 1, 0, 0, "mm")
      )
  
  return(correlation_joint)
}