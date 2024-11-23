# ------------------------------------------------------------------------------
# Alternative method to plot the radars using the "ggradar" package
# ------------------------------------------------------------------------------

# The ggradar package comes from GitHub and needs special treatment
# if (!require("devtools")) install.packages("devtools")
# if (!require("ggradar")) devtools::install_github("ricardo-bion/ggradar")
# 
# if (!requireNamespace("pacman")) install.packages("pacman")
# pacman::p_load(dplyr, here, stringr, tidyr, ggplot2, purrr, see )
# dir_ls(here("R/02_wrangle"), type = "file", recurse = TRUE) |> walk(source)

#' Plotting radars with ggradar using parameter lists
#'
#' @param df The data you want to plot
#' @param new_params A list of parameters that you would like to change in the 
#'                   ggradar function, using ggradar argument names as names in 
#'                   the list. The rest will be my defaults
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
# 
# 
# Old usage --------------------------------------------------------------------
# 
# group_radar_sensory <-
#   df |>  
#   scale_vars() |> 
#   select(group, vviq, osivq_o, contains("psiq")) |>
#   group_by(group) |> 
#   reframe(across(everything(), ~ round(mean(.), digits = 2))) |> 
#   plot_radar(new_params = list(axis.labels = c(
#     "VVIQ","OSIVQ-Object",
#     "Psi-Q Visual", "Psi-Q Audition", 
#     "Psi-Q Smell", "Psi-Q Taste",
#     "Psi-Q Touch", "Psi-Q\nSensations", 
#     "Psi-Q Feelings"
#   )))
# 
# group_radar_others <-
#   df |>  
#   scale_vars() |> 
#   select(group, osivq_s, osivq_v, score_raven:score_comprehension) |>
#   group_by(group) |> 
#   reframe(across(everything(), ~ round(mean(.), digits = 2))) |> 
#   plot_radar(new_params = list(axis.labels = c(
#     "OSIVQ-Spatial","OSIVQ-Verbal",
#     "Raven\nmatrices", "SRI", 
#     "Spatial span", "Digit span",
#     "WCST", "Similarities", "Reading\ncomprehension"
#   )))
# 
# margins <- c(0, 0, 0, 0)
# txt_size_legend <- 18
# 
# group_radar_joint <-
#   group_radar_sensory + group_radar_others +
#   plot_layout(
#     guides = "collect",
#     widths = c(1, 1)
#   ) & 
#   theme(
#     legend.position = "top", 
#     legend.title = element_text(size = txt_size_legend),
#     legend.text  = element_text(size  = txt_size_legend),
#     legend.box.margin = margin(margins)
#   )
# # Clustering radar
# cluster_radar <-
#   df |>
#   add_cluster_vars(clustering) |>
#   select(cluster, visual_imagery:span_digit_std) |> 
#   group_by(cluster) |>
#   reframe(across(everything(), ~ round(mean(.), digits = 2))) |> 
#   plot_radar(new_params = list(
#     axis.labels = c(
#       "Visual\nimagery", 
#       "Spatial\nimagery", 
#       "Verbal\nstrategies",
#       "Non-verbal\nreasoning", 
#       "Verbal\nreasoning", 
#       "Spatial\nspan",
#       "Digit\nspan"
#     ),
#     plot.extent.x.sf = 1.17,
#     plot.extent.y.sf = 1.28,
#     axis.label.offset = 1.25,
#     group.colours = pal_trio
#   )) + 
#   scale_color_manual(
#     values = pal_trio, 
#     name = "Cluster: ",
#     labels = c("A ", "B ", "C ")
#   ) +
#   scale_fill_manual(values = pal_trio, guide = "none")
# 
# # Joint plot
# cluster_plot_joint <-
#   cluster_pca + cluster_radar +
#   plot_layout(
#     guides = "collect",
#     widths = c(0.75, 1)
#   ) &
#   theme(
#     legend.position = "top", 
#     legend.title = element_text(size = txt_size_legend),
#     legend.text  = element_text(size = txt_size_legend),
#     legend.box.margin = margin(0,20,0,0)
#   )



































