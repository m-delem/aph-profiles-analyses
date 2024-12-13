# if (!requireNamespace("pacman")) install.packages("pacman")
pacman::p_load(ggbeeswarm, ggplot2, see)

# Plot the cluster scores on all reduced variables and WCST/Reading as violins
plot_clusters_violins <- function(
    df, clustering, var,
    txt_size_strip = 14,
    txt_size_legend = 14,
    legend_margins = c(0, 0, 0, 0),
    panel_spacing_y = 0.1,
    size = 1,
    lw = 1,
    lw_sd = 1,
    lw_bg = 0.5
) {
  # Defining all the contrasts to display for clusters or subclusters (lengthy)
  up <- function(y, n = 1, gap = 0.03) y + n * gap
  y0 = 1.03
  
  cluster_effects <- 
    tibble(
      Variable = c(
        rep("Visual imagery", 3),
        rep("Auditory imagery", 3),
        rep("Sensory imagery", 3),
        rep("Spatial imagery", 3),
        rep("Verbal strategies", 3),
        rep("Verbal reasoning", 3),
        rep("Spatial span", 3)
      ) |> factor() |> fct_inorder(),
      x_star = c(
        1.5, 2.5, 2.5,
        1.5, 2.5, 2.5,
        1.5, 2.5, 2.5,
        1.5, 0.0, 2.5,
        0.0, 1.5, 2.5,
        1.5, 0.0, 0.0,
        1.5, 0.0, 0.0
      ),
      y_star = c(
        up(y0, 2), up(y0, 1), 0.8,
        up(y0, 3), up(y0, 2), y0,
        up(y0, 3), up(y0, 2), y0,
        up(y0, 1), 0,         up(y0, 1),
        0,         up(y0, 1), up(y0, 2),
        up(y0, 1), 0,         0,
        up(y0, 1), 0,         0
      ),
      stars = c(
        "***", "***", "***",
        "***", "***", "   ",
        "***", "***", "   ",
        "***", "   ", "***",
        "   ", "***", "***",
        "***", "   ", "   ",
        "***", "   ", "   "
      ),
      x_line = c(
        1.0, 1.0, 2.0,
        1.0, 1.0, 2.0,
        1.0, 1.0, 2.0,
        1.0, 0.0, 2.1,
        0.0, 1.0, 2.0,
        1.0, 0.0, 0.0,
        1.0, 0.0, 0.0
      ),
      x_line_end = c(
        2.0, 3.0, 3.0,
        2.0, 3.0, 3.0,
        2.0, 3.0, 3.0,
        1.9, 0.0, 3.0,
        0.0, 3.0, 3.0,
        2.0, 0.0, 0.0,
        2.0, 0.0, 0.0
      ),
      y_line = c(
        up(y0, 1), up(y0, 0), 0.77,
        up(y0, 2), up(y0, 1), up(y0, 0),
        up(y0, 2), up(y0, 1), up(y0, 0),
        up(y0, 0), 0,         up(y0, 0),
        0,         up(y0, 0), up(y0, 1),
        up(y0, 0), 0,         0,
        up(y0, 0), 0,         0
      )
    )
  
  subcluster_effects <- 
    tibble(
      Variable = c(
        rep("Visual imagery", 6),
        rep("Auditory imagery", 6),
        rep("Sensory imagery", 6),
        rep("Spatial imagery", 6),
        rep("Verbal strategies", 6),
        rep("Verbal reasoning", 6),
        rep("Spatial span", 6)
      ) |> factor() |> fct_inorder(),
      stars = c(
        "***", "***", "***", "***", "***", "   ",
        "   ", "***", "***", "   ", "   ", "   ",
        "   ", "***", "***", "   ", "   ", "   ",
        "***", "***", "   ", "   ", "***", "*",
        "   ", "   ", "***", "   ", "***", "***",
        "*",   "**",  "   ", "   ", "   ", "   ",
        "***", "***", "   ", "   ", "   ", "   "
      ),
      x_star = c(
        1.5, 2.5, 3.5, 2.5, 3.5, 0.0,
        0.0, 2.5, 3.5, 0.0, 0.0, 0.0,
        0.0, 2.5, 3.5, 0.0, 0.0, 0.0,
        1.5, 1.5, 0.0, 0.0, 3.5, 3.5,
        0.0, 0.0, 1.5, 0.0, 2.5, 3.5,
        1.5, 2.5, 0.0, 0.0, 0.0, 0.0,
        1.5, 2.5, 0.0, 0.0, 0.0, 0.0
      ),
      y_star = c(
        up(y0, 3), up(y0, 2), up(y0, 1), 0.79, 0.76, 0.0,
        0.0, up(y0, 4), up(y0, 4), 0.0, 0.0, 0.0,
        0.0, up(y0, 4), up(y0, 4), 0.0, 0.0, 0.0,
        0.93, up(y0, 1), 0.0, 0.0, up(y0, 2), 0.93,
        0.0, 0.0, up(y0, 1), 0.0, up(y0, 2), up(y0, 3),
        up(y0, 2), up(y0, 1), 0.0, 0.0, 0.0, 0.0,
        up(y0, 2), up(y0, 1), 0.0, 0.0, 0.0, 0.0
      ),
      x_line = c(
        1.0, 1.0, 1.0, 2.0, 2.0, 0.0,
        0.0, 1.0, 1.0, 2.0, 2.0, 3.1,
        0.0, 1.0, 1.0, 2.0, 2.0, 3.1,
        1.0, 1.0, 0.0, 0.0, 2.0, 3.0,
        0.0, 0.0, 1.0, 0.0, 2.0, 3.0,
        1.0, 1.0, 0.0, 0.0, 0.0, 0.0,
        1.0, 1.0, 0.0, 0.0, 0.0, 0.0
      ),
      x_line_end = c(
        2.0, 3.0, 4.0, 3.0, 4.0, 0.0,
        0.0, 3.0, 4.0, 3.0, 4.0, 4.0,
        0.0, 3.0, 4.0, 3.0, 4.0, 4.0,
        2.0, 3.0, 0.0, 0.0, 4.0, 4.0,
        0.0, 0.0, 4.0, 0.0, 4.0, 4.0,
        2.0, 3.0, 0.0, 0.0, 0.0, 0.0,
        2.0, 3.0, 0.0, 0.0, 0.0, 0.0
      ),
      y_line = c(
        up(y0, 2), up(y0, 1), up(y0, 0), 0.76, 0.73, 0.0,
        0.0,       up(y0, 1), up(y0, 0), up(y0, 3), up(y0, 2), up(y0, 3),
        0.0,       up(y0, 1), up(y0, 0), up(y0, 3), up(y0, 2), up(y0, 3),
        0.9,       up(y0, 0), 0.0, 0.0, up(y0, 1), 0.9,
        0.0,       0.0, up(y0, 0), 0.0, up(y0, 1), up(y0, 2),
        up(y0, 1), up(y0, 0), 0.0, 0.0, 0.0, 0.0,
        up(y0, 1), up(y0, 0), 0.0, 0.0, 0.0, 0.0
      )
    )
  
  
  choice <- deparse(substitute(var))
  
  if (choice == "Cluster") {
    pal <- c("#56B4E9", "#E69F00", "#009E73")
    effects <- cluster_effects
    
  } else if (choice == "Subcluster"){
    pal <- c("#56B4E9", "#E69F00", "#F5C710", "#009E73")
    effects <- subcluster_effects
    
  } else {
    stop(
      "Invalid group variable. ",
      "Choose between 'Cluster', or 'Subcluster'.")
  }
  
  p <- 
    df |> 
    add_cluster_vars(clustering) |> 
    scale_vars() |>  
    select(
      cluster, 
      subcluster,
      visual_imagery,
      auditory_imagery = psiq_aud, 
      sensory_imagery:spatial_span, 
      wcst_accuracy, score_comprehension
    ) |> 
    get_long_format() |> 
    group_by({{ var }}, Variable) |>
    mutate(
      Cluster = case_when(
        Cluster == "Cluster A" ~ "A (Control)   ",
        Cluster == "Cluster B" ~ "B (Mixed)   ",
        Cluster == "Cluster C" ~ "C (Aphantasic)",
        TRUE ~ Cluster
      ),
      Subcluster = fct_relabel(Subcluster, ~ paste(., "   ")),
      mean = round(mean(value), digits = 2),
      sd = round(sd(value), digits = 2)
    ) |> 
    # --------------------------------------------------------------------
    ggplot(aes(y = value, x = {{ var }}, color = {{ var }}, fill = {{ var }})) +
    geom_violinhalf(
      alpha = .3, 
      scale = "width", 
      color = "transparent"
    ) +
    geom_quasirandom(
      width = 0.15, 
      alpha = 0.3, 
      size = 1, 
      show.legend = FALSE
    ) +
    geom_line(
      aes(x = {{ var }}, y = mean, group = 1),
      color = "grey80",
      linewidth = lw
    ) +
    geom_pointrange2(
      aes(
        x = {{ var }}, 
        y = mean,
        ymin = if_else(mean - sd <= 0, 0, mean - sd),
        ymax = if_else(mean + sd >= 1, 1, mean + sd),
        group = {{ var }}
      ),
      show.legend = FALSE,
      # color       = "black",
      size        = size,
      linewidth   = lw_sd
    ) +
    # ----------------------------
    add_significance(effects) +
    # ----------------------------
    labs(
      x = NULL,
      y = "Standardised scores",
      fill = NULL
    ) +
    scale_y_continuous(
      expand = expansion(c(0.05, 0)), 
      limits = c(0, 1.25),
      breaks = seq(0, 1, .2)
    ) +
    scale_colour_manual(values = pal) +
    scale_fill_manual(values   = pal) +
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
      panel.border       = element_rect(color = "grey80", fill = NA),
      panel.spacing.x    = unit(0, "in"),
      panel.spacing.y    = unit(panel_spacing_y, "in"),
      strip.text         = element_text(size = txt_size_strip, face = "plain"),
      strip.background   = element_rect(
        fill      = "grey95",
        color     = "grey80",
        linewidth = lw_bg
      ),
    )
  
  return(p)
}