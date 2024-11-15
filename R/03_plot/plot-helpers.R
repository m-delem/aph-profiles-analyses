# Adding significance labels above plots ----------------------------------

add_significance <- function(
    variables, 
    stars = "*", size_star = 6, 
    x_star = 1.5, y_star = 1.08, 
    x_line = 1, x_line_end = 2,
    y_line = 1.05, lw = 0.5){
  
  list(
    geom_text(
      data = data.frame(Variable = variables),
      inherit.aes = FALSE,
      x = x_star,
      y = y_star,
      color = "black",
      label = stars,
      size = size_star
    ),
    geom_segment(
      data = data.frame(Variable = variables),
      inherit.aes = FALSE,
      x = x_line,
      xend = x_line_end,
      y = y_line,
      yend = y_line,
      color = "black",
      linewidth = lw
    )
  )
}


# Theme for violin plots --------------------------------------------------

theme_violins <- function(
    txt_size_sub_plots = 14,
    txt_size_legend = 18,
    legend_margins = c(0, 0, 0, 0),
    panel_spacing_y = 0.2,
    lw = 0.5
){
  list(
    theme(
      legend.position = "top",
      legend.title = element_text(size = txt_size_legend),
      legend.text  = element_text(size = txt_size_legend),
      legend.box.margin = margin(legend_margins),
      axis.title.y = element_text(size = txt_size_legend),
      axis.text.y  = element_text(size = txt_size_sub_plots),
      axis.text.x  = element_blank(),
      axis.line    = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(),
      panel.grid.minor.y = element_line(),
      panel.spacing.x  = unit(0, "in"),
      panel.spacing.y  = unit(panel_spacing_y, "in"),
      strip.text = element_text(size = txt_size_sub_plots, face = "plain"),
      strip.background = element_rect(
        fill = "grey95",
        color = "grey80",
        linewidth = lw
      ),
      panel.border = element_rect(color = "grey80", fill = NA)
    )
  )
}
