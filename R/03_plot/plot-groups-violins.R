if (!requireNamespace("pacman")) install.packages("pacman")
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
source(here("R/02_wrangle/get-long-format.R"))
source(here("R/02_wrangle/transform-vars.R"))
source(here("R/03_plot/plot-helpers.R"))

plot_groups_violins <- function(
    df, 
    txt_size_strip = 14,
    txt_size_legend = 18,
    legend_margins = c(0, 0, 0, 0),
    panel_spacing_y = 0.1,
    palette = c("#56B4E9", "#009E73"),
    size = 1,
    lw = 1,
    lw_sd = 1.2,
    lw_bg = 0.5
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
      stars = "***",
      x_line = x_star - 0.5,
      x_line_end = x_star + 0.5
    )
  
  group_effect_verbal <- 
    tibble(
      Variable = factor("OSIVQ-Verbal"),
      x_star = 1.5,
      stars = "**",
      x_line = x_star - 0.5,
      x_line_end = x_star + 0.5
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
      size = 1, 
      show.legend = FALSE
    ) +
    geom_line(
      aes(x = Group, y = mean, group = 1),
      color = "grey80",
      linewidth = lw
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
      size        = size,
      linewidth   = lw_sd
    ) +
    # -------------------------------------
    add_significance(group_effects) +
    add_significance(group_effect_verbal) +
    # -------------------------------------
    scale_y_continuous(
      expand = expansion(c(0.05, 0.15)), 
      limits = c(0, 1),
      breaks = seq(0, 1, .2)
    ) +
    scale_color_manual(values = palette, name = "", labels = labels) +
    scale_fill_manual(values  = palette, name = "", labels = labels) +
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
      legend.title       = element_text(size = txt_size_legend),
      legend.text        = element_text(size = txt_size_legend),
      legend.box.margin  = margin(legend_margins),
      axis.title.y       = element_text(size = txt_size_legend),
      axis.text.y        = element_text(size = txt_size_strip - 4),
      axis.ticks.y       = element_line(colour = "grey80"),
      axis.text.x        = element_blank(),
      axis.ticks.x       = element_blank(), 
      axis.line          = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(),
      panel.grid.minor.y = element_line(),
      panel.spacing.x    = unit(0, "in"),
      panel.spacing.y    = unit(panel_spacing_y, "in"),
      strip.text         = element_text(size = txt_size_strip, face = "plain"),
      strip.background   = element_rect(
        fill      = "grey95",
        color     = "grey80",
        linewidth = lw_bg
      ),
      panel.border = element_rect(color = "grey80", fill = NA)
    )
  
  return(p)
}

