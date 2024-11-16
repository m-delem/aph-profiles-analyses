# Adding significance labels above plots ----------------------------------

add_significance <- function(
    df_effects, 
    size_star = 6, 
    y_star = 1.08, 
    y_line = 1.05, 
    lw = 0.5
){
  list(
    geom_text(
      data = df_effects,
      aes(
        x = x_star,
        label = stars
      ),
      y = y_star,
      size = size_star,
      inherit.aes = FALSE,
      color = "black"
    ),
    geom_segment(
      data = df_effects,
      aes(
        x = x_line,
        xend = x_line_end,
      ),
      inherit.aes = FALSE,
      color = "black",
      y = y_line,
      yend = y_line,
      linewidth = lw
    )
  )
}
