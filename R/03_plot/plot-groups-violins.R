# if (!requireNamespace("pacman")) install.packages("pacman")
pacman::p_load(
  dplyr, 
  here,
  forcats,
  ggbeeswarm,
  ggplot2, 
  see,
  stringr,
  tibble
)

# Plot the group scores on all variables as violins
plot_groups_violins <- function(
    df, 
    palette = c("#56B4E9", "#009E73"),
    lw_bg = 0.1
) {
  # Defining the effects to label with stars
  group_effects <-
    tibble(
      Variable = factor(c(
        "VVIQ","Psi-Q Vision",
        "Psi-Q Audition", "Psi-Q Smell", "Psi-Q Taste",
        "Psi-Q Touch", "Psi-Q Sensations", "Psi-Q Feelings",
        "OSIVQ-Object")) |> fct_inorder(),
      x_star = 1.5,
      y_star = 1.08,
      stars = "***",
      x_line = x_star - 0.5,
      x_line_end = x_star + 0.5,
      y_line = 1.05
    )
  
  group_effect_verbal <- 
    tibble(
      Variable = factor("OSIVQ-Verbal"),
      x_star = 1.5,
      y_star = 1.08,
      stars = "**",
      x_line = x_star - 0.5,
      x_line_end = x_star + 0.5,
      y_line = 1.05
    )
  
  # For the legend
  labels <- c(" Control    ", " Aphantasic")
  
  p <- 
    df |> 
    scale_vars() |>
    get_long_format() |> 
    group_by(Group, Variable) |>
    reframe(value = value, mean = mean(value), sd = sd(value)) |> 
    mutate(
      Variable = 
        Variable |> 
        str_replace("Reading comprehension", "Reading\ncomprehension") |>
        fct_relevel(
          levels(group_effects$Variable), "OSIVQ-Spatial", "OSIVQ-Verbal",
          "Raven matrices", "SRI", "Spatial span", "Digit span",
          "Similarities test", "WCST", "Reading\ncomprehension"
        )
    ) |> 
    # --------------------------------------------------------------
    ggplot(aes(y = value, x = Group, color = Group, fill = Group)) +
    geom_violinhalf(
      flip  = c(1),
      alpha = .3, 
      scale = "width", 
      color = "transparent",
      # show.legend = FALSE
    ) +
    geom_quasirandom(
      width = 0.15, 
      alpha = 0.3, 
      size  = 0.25, 
      show.legend = FALSE
    ) +
    geom_line(
      aes(x = Group, y = mean, group = 1),
      color = "grey80",
      linewidth = 0.3
    ) +
    geom_pointrange2(
      aes(
        x = Group, 
        y = mean,
        ymin = if_else(mean - sd <= 0, 0, mean - sd),
        ymax = if_else(mean + sd >= 1, 1, mean + sd),
        group = Group
      ),
      # show.legend = FALSE,
      # color       = "black",
      size        = 0.3,
      linewidth   = 0.3
    ) +
    # -------------------------------------
    add_significance(group_effects) +
    add_significance(group_effect_verbal) +
    # -------------------------------------
    scale_y_continuous(
      expand = expansion(c(0.05, 0)), 
      limits = c(0, 1.15),
      breaks = seq(0, 1, .2)
    ) +
    scale_color_manual(values = palette, labels = labels) +
    scale_fill_manual(values  = palette, labels = labels) +
    labs(
      x = NULL,
      y = "Standardised scores"
    ) +
    # -------------------------------
    facet_wrap(~Variable, nrow = 2) +
    # -------------------------------
    theme_modern() +
    theme(
      legend.position    = "top",
      legend.title       = element_blank(),
      legend.text        = element_text(size = 7),
      legend.box.margin  = margin(1, 0, 0, 0, "mm"),
      legend.box.spacing = unit(1, "mm"),
      legend.margin = margin(0, 0, 0, 0, "mm"),
      axis.title.y       = element_text(
        size = 7, 
        margin = margin(0, 1.5, 0, 0, "mm")
      ),
      axis.text.y        = element_text(size = 5),
      axis.ticks.y       = element_line(colour = "grey80", linewidth = lw_bg),
      axis.text.x        = element_blank(),
      axis.ticks.x       = element_blank(), 
      axis.line          = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(linewidth = lw_bg),
      panel.grid.minor.y = element_line(linewidth = lw_bg),
      panel.spacing.x    = unit(0, "mm"),
      panel.spacing.y    = unit(3, "mm"),
      strip.text         = element_text(size = 6, face = "plain"),
      strip.background   = element_rect(
        fill      = "grey95",
        color     = "grey80",
        linewidth = lw_bg
      ),
      panel.border = element_rect(color = "grey80", fill = NA),
      plot.margin = margin(0, 1, 0, 1, "mm")
    )
  
  return(p)
}

