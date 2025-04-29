# Plot the group scores on all variables as violins
plot_violins <- function(
    df_long, 
    var_selection = "original",
    palette  = c("#56B4E9", "#009E73"),
    txt_big  = 7,
    txt_mid  = 6,
    txt_smol = 5,
    dot_big  = 0.3,
    dot_smol = 0.15,
    lw_big   = 0.1,
    lw_smol  = 0.1,
    alpha    = 0.3
) {
  withr::local_options(list(warn = -1))
  
  # Sub-function: Add significance labels above plots --------------------------
  add_significance <- function(
    df_effects, 
    size_star = 2.5, 
    lw = 0.2
  ){
    # df_effects must contain:
    # - Variable
    # - x_star
    # - y_star
    # - stars
    # - x_line
    # - x_line_end
    # - y_line
    list(
      ggplot2::geom_text(
        data = df_effects,
        ggplot2::aes(
          x     = x_star,
          y     = y_star,
          label = stars
        ),
        size        = size_star,
        color       = "black",
        inherit.aes = FALSE
      ),
      ggplot2::geom_segment(
        data = df_effects,
        ggplot2::aes(
          x    = x_line,
          xend = x_line_end,
          y    = y_line,
          yend = y_line,
        ),
        color       = "black",
        linewidth   = lw,
        inherit.aes = FALSE
      )
    )
  }
  
  # Defining the effects to label with stars -----------------------------------
  group_effects <-
    tibble::tibble(
      Variable = factor(c(
        "VVIQ","Psi-Q Vision",
        "Psi-Q Audition", "Psi-Q Smell", "Psi-Q Taste",
        "Psi-Q Touch", "Psi-Q Sensations", "Psi-Q Feelings",
        "OSIVQ-Object")) |> forcats::fct_inorder(),
      x_star = 1.5,
      y_star = 1.08,
      stars  = "***",
      x_line = x_star - 0.5,
      x_line_end = x_star + 0.5,
      y_line = 1.05
    )
  
  group_effect_verbal <- 
    tibble::tibble(
      Variable   = factor("OSIVQ-Verbal"),
      x_star     = 1.5,
      y_star     = 1.08,
      stars      = "**",
      x_line     = x_star - 0.5,
      x_line_end = x_star + 0.5,
      y_line     = 1.05
    )
  
  # Variable selections --------------------------------------------------------
  if (var_selection == "original") {
    vars <- c(
      # Original scores
      "VVIQ",
      "OSIVQ-Object",
      "OSIVQ-Spatial",
      "OSIVQ-Verbal",
      "Psi-Q Vision",
      "Psi-Q Audition",
      "Psi-Q Smell",
      "Psi-Q Taste",
      "Psi-Q Touch",
      "Psi-Q Sensations",
      "Psi-Q Feelings",
      "Raven matrices",
      "SRI",
      "Digit span",
      "Spatial span",
      "Similarities test",
      # Complex tasks
      "WCST",
      "Reading\ncomprehension"
    )
  } else if (var_selection == "reduced") {
    vars <- c(
      # Reduced variables
      "Visual imagery",
      "Auditory imagery",
      "Sensory imagery",
      "Spatial imagery",
      "Verbal strategies",
      "Raven +\nDigit Span",
      # "Non-verbal\nreasoning",
      "Verbal reasoning",
      "Spatial span std.",
      # Complex tasks
      "WCST",
      "Reading\ncomprehension"
    )
  } else {
    stop("var_selection must be either 'original' or 'reduced'.")
  }
  
  # Main plot ------------------------------------------------------------------
  p <- 
    df_long |>
    dplyr::filter(Variable %in% vars) |>
    dplyr::group_by(Group, Variable) |>
    dplyr::reframe(value = value, mean = mean(value), sd = sd(value)) |> 
    dplyr::mutate(
      Variable = 
        Variable |> 
        stringr::str_replace(
          "Reading comprehension", "Reading\ncomprehension"
        ) |>
        forcats::fct_relevel(
          levels(group_effects$Variable), "OSIVQ-Spatial", "OSIVQ-Verbal",
          "Raven matrices", "SRI", "Spatial span", "Digit span",
          "Similarities test", "WCST", "Reading\ncomprehension"
        )
    ) |> 
    # --------------------------------------------------------------
  ggplot2::ggplot(aes(y = value, x = Group, color = Group, fill = Group)) +
    see::geom_violinhalf(
      flip  = c(1),
      alpha = alpha, 
      scale = "width", 
      color = "transparent",
      # show.legend = FALSE
    ) +
    ggbeeswarm::geom_quasirandom(
      width = 0.15, 
      shape = 16,
      alpha = alpha, 
      size  = dot_big, 
      show.legend = FALSE
    ) +
    ggplot2::geom_line(
      mapping   = aes(x = Group, y = mean, group = 1),
      color     = "grey80",
      linewidth = lw_big
    ) +
    see::geom_pointrange2(
      ggplot2::aes(
        x     = Group, 
        y     = mean,
        ymin  = ifelse(mean - sd <= 0, 0, mean - sd),
        ymax  = ifelse(mean + sd >= 1, 1, mean + sd),
        group = Group
      ),
      size        = dot_big,
      linewidth   = lw_big
    ) +
    # -------------------------------------
    add_significance(group_effects) +
    add_significance(group_effect_verbal) +
    # -------------------------------------
    ggplot2::scale_y_continuous(
      expand = expansion(c(0.05, 0)), 
      limits = c(0, 1.15),
      breaks = seq(0, 1, .2)
    ) +
    ggplot2::scale_color_manual(values = palette) +
    ggplot2::scale_fill_manual(values  = palette) +
    ggplot2::labs(y = "Standardised scores") +
    # -------------------------------
    ggplot2::facet_wrap(~Variable, nrow = 2) +
    # -------------------------------
    see::theme_modern() +
    ggplot2::theme(
      # whole plot
      plot.margin        = ggplot2::margin(0, 1, 0, 1, "mm"),
      # legend
      legend.position    = "top",
      legend.title       = ggplot2::element_blank(),
      legend.text        = ggplot2::element_text(size = txt_big),
      legend.margin      = ggplot2::margin(0, 0, 0, 0, "mm"),
      legend.box.margin  = ggplot2::margin(1, 0, 0, 0, "mm"),
      legend.box.spacing = grid::unit(1, "mm"),
      # y axis
      axis.title.y       = ggplot2::element_text(
        size   = txt_big, 
        margin = margin(0, 1.5, 0, 0, "mm")
      ),
      axis.text.y        = ggplot2::element_text(size = txt_smol),
      axis.ticks.y       = ggplot2::element_line(
        colour = "grey92", linewidth = lw_smol
      ),
      # x axis
      axis.title.x       = ggplot2::element_blank(),
      axis.text.x        = ggplot2::element_blank(),
      axis.ticks.x       = ggplot2::element_blank(), 
      axis.line          = ggplot2::element_blank(),
      # panel lines
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(linewidth = lw_smol),
      panel.grid.minor.y = ggplot2::element_line(linewidth = lw_smol),
      # facets
      panel.border       = ggplot2::element_rect(
        color     = "grey92", 
        fill      = NA, 
        linewidth = lw_smol
      ),
      panel.spacing.x    = grid::unit(0, "mm"),
      panel.spacing.y    = grid::unit(3, "mm"),
      strip.text         = ggplot2::element_text(
        size = txt_mid, face = "plain"
      ),
      strip.background   = ggplot2::element_rect(
        color     = "grey92",
        fill      = "grey98",
        linewidth = lw_smol
      )
    )
  
  return(p)
}

