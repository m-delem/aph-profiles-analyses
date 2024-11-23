# Add significance labels above plots
add_significance <- function(
    df_effects, 
    size_star = 6, 
    lw = 0.5
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
    geom_text(
      data = df_effects,
      aes(
        x     = x_star,
        y     = y_star,
        label = stars
      ),
      size        = size_star,
      color       = "black",
      inherit.aes = FALSE
    ),
    geom_segment(
      data = df_effects,
      aes(
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
