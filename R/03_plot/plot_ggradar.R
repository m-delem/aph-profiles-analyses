# The ggradar package comes from GitHub and needs special treatment
# if (!require("devtools")) install.packages("devtools")
# if (!require("ggradar")) devtools::install_github("ricardo-bion/ggradar")

#' Plotting radars with ggradar using parameter lists
#'
#' @param df The data you want to plot
#' @param new_params A list of parameters that you would like to change in the ggradar function, using ggradar argument names as names in the list. The rest will be my defaults
#'
#' @return A ggradar radar plot
#' @export
#'
#' @examples
# plot_radar <- function(df, new_params) {
#   
#   # Default ggradar parameters I chose
#   radar_params <- c(
#     list(values.radar = c("0","0.5","1")),
#     list(axis.labels = c(
#       "Your variable labels"
#     )),
#     grid.label.size = 4,
#     plot.extent.x.sf = 1.25,
#     plot.extent.y.sf = 1.15,
#     grid.min = 0, grid.mid = .5, grid.max = 1,
#     label.gridline.min = FALSE,
#     group.line.width = .5, group.point.size = 3,
#     background.circle.transparency = .1,
#     legend.title = "",
#     list(group.colours = pal_duo),
#     axis.label.size = 5.5,
#     fill = TRUE,
#     fill.alpha = 0.05
#   )
#   
#   do.call(ggradar, modifyList(radar_params, c(list(plot.data = df), new_params)))
# }