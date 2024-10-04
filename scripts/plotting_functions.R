# Plotting functions shared across scripts are placed here


# Plotting radars --------------------------------------------------------------

#' Plotting radars with ggradar using parameter lists
#'
#' @param df The data you want to plot
#' @param new_params A list of parameters that you would like to change in the ggradar function, using ggradar argument names as names in the list. The rest will be my defaults
#'
#' @return A ggradar radar plot
#' @export
#'
#' @examples
plot_radar <- function(df, new_params) {
  
  # Default ggradar parameters I chose
  radar_params <- c(
    list(values.radar = c("0","0.5","1")),
    list(axis.labels = c(
      "Your variable labels"
    )),
    grid.label.size = 4,
    plot.extent.x.sf = 1.25,
    plot.extent.y.sf = 1.15,
    grid.min = 0, grid.mid = .5, grid.max = 1,
    label.gridline.min = FALSE,
    group.line.width = .5, group.point.size = 3,
    background.circle.transparency = .1,
    legend.title = "",
    list(group.colours = pal_duo),
    axis.label.size = 5.5,
    fill = TRUE,
    fill.alpha = 0.05
  )
  
  do.call(ggradar, modifyList(radar_params, c(list(plot.data = df), new_params)))
}


# Adding significance labels above plots ----------------------------------

add_significance <- function(
    variables, 
    stars = "*", size_star = 6, y_star = 1.08, 
    y_bar = 1.05, lw = 0.5){
  
  list(
    geom_text(
      data = data.frame(Variable = variables),
      inherit.aes = FALSE,
      x = 1.5,
      y = y_star,
      color = "black",
      label = stars,
      size = size_star
    ),
    geom_segment(
      data = data.frame(Variable = variables),
      inherit.aes = FALSE,
      x = 1,
      xend = 2,
      y = y_bar,
      yend = y_bar,
      color = "black",
      linewidth = lw
    )
  )
}


# Theme for violin plots --------------------------------------------------

theme_violins <- function(
    txt_size_sub_plots = 14,
    txt_size_legend = 18,
    legend_margins = c(0, 0, 10, 0),
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
      panel.spacing.y  = unit(0.3, "in"),
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
