if (!requireNamespace("pacman")) install.packages("pacman")
pacman::p_load(
  dplyr, 
  here, 
  forcats,
  ggplot2, 
  patchwork,
  see,
  stringr, 
  superb, 
  tibble
)
source(here("R/02_wrangle/get-long-format.R"))
source(here("R/02_wrangle/transform-vars.R"))
source(here("R/03_plot/plot-helpers.R"))

plot_groups_superb <- function(
    df, 
    palette = c("#56B4E9", "#009E73"),
    # vars = "VVIQ",
    angle = 30,
    vjust = 1,
    hjust = 1
) {
  group_effects <-
    tibble(
      Variable = factor(c(
        "VVIQ","Psi-Q Vision",
        "Psi-Q Audition", "Psi-Q Smell", "Psi-Q Taste",
        "Psi-Q Touch", "Psi-Q Sensations", "Psi-Q Feelings",
        "OSIVQ-Object")) |> fct_inorder(),
      x_star = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
      stars = "***",
      x_line = x_star - 0.1,
      x_line_end = x_star + 0.1
    )
  
  group_effect_verbal <- 
    tibble(
      Variable = factor("OSIVQ-Verbal"),
      x_star = 2,
      stars = "**",
      x_line = x_star - 0.1,
      x_line_end = x_star + 0.1
    )
  
  plot_superb <- function(df, vars) {
    df_long <- 
      df |> 
      scale_vars() |> 
      get_long_format() |> 
      mutate(Variable = fct_recode(
        Variable, 
        "Reading\ncomprehension" = "Reading comprehension"
      )
      ) |> 
      select(Group, Variable, value) |> 
      filter(str_detect(Variable, vars))
    
    p <- 
      superb(
        formula = value ~ Variable + Group,
        data = df_long,
        factorOrder    = c("Variable", "Group"),
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
      scale_y_continuous(
        expand = expansion(c(0, 0.18)),
        limits = c(0, 1),
        breaks = seq(0, 1, 0.2)
      ) +
      scale_colour_manual(values = palette) +
      scale_fill_manual(values   = palette) +
      labs(x = NULL, y = "Standardized score") +
      theme_modern() +
      theme(
        legend.position = "top",
        legend.title = element_text(size = 16),
        legend.text  = element_text(size = 16),
        # legend.box.margin = margin(0, 0, 0, 0, "cm"),
        panel.grid.major.y = element_line(color = "grey90"),
        panel.grid.minor.y = element_line(color = "grey95"),
        axis.text.x = element_text(size = 14, angle = angle, vjust = vjust, hjust = hjust),
        axis.ticks.x = element_line(),
        axis.title.y = element_text(size = 16),
        # panel.border = element_rect(color = "black", fill = NA, size = 0.5)
      )
    
    return(p)
  }
  
  p_groups_superb_1 <- 
    plot_superb(
      df,
      vars = "VVIQ|OSIVQ-Object|Psi-Q"
    ) + 
    add_significance(group_effects) +
    theme(
      # plot.margin = margin(0, 0, .5, 0, "cm")
      )
  
  p_groups_superb_2 <- 
    plot_superb(
      df,
      vars = paste0(
        "OSIVQ-Spatial|OSIVQ-Verbal|Raven matrices|SRI",
        "|Spatial span|Digit span|Similarities test|",
        "WCST|Reading\ncomprehension"
      )
    ) +
    add_significance(group_effect_verbal)
  
  p_groups_superb <- 
    p_groups_superb_1 / p_groups_superb_2 + 
    plot_layout(guides = "collect") &
    theme(
      legend.position = "top",
      legend.title = element_blank(),
      legend.text  = element_text(size = 18),
      axis.text.x = element_text(size = 16),
    )
  
  return(p_groups_superb)
}