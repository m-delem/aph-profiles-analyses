# if (!requireNamespace("pacman")) install.packages("pacman")
pacman::p_load(ggplot2)

# Custom ggsave calls set with Nature's formatting guidelines ------------------
ggsave_single_col <- function(path, plot, height) {
  ggsave(
    here(path),
    plot   = plot,
    width  = 88,      # standard width for single column figures
    height = height,
    units  = "mm",
    dpi    = 600
  )
}

ggsave_double_col <- function(path, plot, height) {
  ggsave(
    here(path),
    plot   = plot,
    width  = 180,    # standard width for double column figures
    height = height,
    units  = "mm",
    dpi    = 600
  )
}