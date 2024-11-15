if (!requireNamespace("pacman")) install.packages("pacman")
pacman::p_load(dplyr, here, stringr, tidyr, ggplot2, purrr, superb, see)
dir_ls(here("R/02_wrangle"), type = "file", recurse = TRUE) |> walk(source)

plot_groups_superb <- function(
    df, 
    palette = c("#56B4E9", "#009E73"),
    vars = "VVIQ|OSIVQ-Object|Psi-Q"
) {
  df_long <- 
    df |> 
    scale_vars() |> 
    get_long_format() |> 
    select(Group, Variable, value) |> 
    filter(str_detect(Variable, vars))
  
  superb(
    formula = value ~ Variable + Group,
    data = df_long,
    plotStyle      = "pointlinejitter",        
    lineParams     = list(linewidth = 1.2, alpha = 0.3),
    errorbarParams = list(linewidth = 1.2, width = 0),
    jitterParams   = list(size = 1, alpha = 0.33)
  ) + 
    geom_violinhalf(
      data = df_long,
      aes(fill = Group, x = Variable, y = value), 
      color = "transparent",
      flip = c(1, 3, 5, 7, 9, 11, 13, 15, 17),
      alpha = .2, 
      scale = "width",
      show.legend = FALSE
    ) +
    scale_y_continuous(expand = expansion(0)) +
    scale_colour_manual(values = palette) +
    scale_fill_manual(values   = palette) +
    labs(x = NULL, y = "Standardized score") +
    theme_modern() +
    theme(
      legend.position = "top",
      legend.title = element_text(size = 16),
      legend.text  = element_text(size = 16),
      legend.box.margin = margin(0, 0, 0, 0, "cm"),
      panel.grid.major.y = element_line(color = "grey90"),
      panel.grid.minor.y = element_line(color = "grey95"),
      axis.text.x = element_text(size = 14, angle = 15, vjust = .5, hjust = .6),
      axis.ticks.x = element_line(),
      axis.title.y = element_text(size = 16),
    )
}