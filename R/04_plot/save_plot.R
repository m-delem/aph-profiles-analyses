# Custom ggsave calls set with major publications' formatting guidelines -------
save_plot <- function(plot, path, ncol = 1, height) {
  if (ncol == 1) {
    width <- 88
  } else if (ncol == 2) {
    width <- 180
  } else stop(glue::glue_col("ncol must be {cyan 1} or {green 2}."))
  
  ggplot2::ggsave(
    plot   = plot,
    here::here(path),
    width  = width,      # standard width for single column figures
    height = height,
    units  = "mm",
    dpi    = 600
  )
}