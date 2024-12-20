# if (!requireNamespace("pacman")) install.packages("pacman")
pacman::p_load(dplyr, forcats, glue, ggplot2, see, stringr, superb, withr)

plot_radars <- function(
    df_long, 
    groups,
    var_selection = "reduced", # or "original"
    txt_big  = 7,
    txt_mid  = 6,
    txt_smol = 5,
    dot_size = 0.8,
    lw       = 0.2,
    y_off    = 40, # to center the y axis text
    r_off    = 0,
    l_off    = 0,
    v_off    = 0,
    key      = 3,
    ...
  ) {
  withr::local_options(list(warn = -1))
  
  # Variable selections --------------------------------------------------------
  if (var_selection == "original") {
    vars <- c(
      # Original scores
      "VVIQ",
      "OSIVQ-Object",
      "OSIVQ-Spatial",
      "OSIVQ-Verbal",
      "Psi-Q Vision",
      "Psi-Q Audition",
      "Psi-Q Smell",
      "Psi-Q Taste",
      "Psi-Q Touch",
      "Psi-Q Sensations",
      "Psi-Q Feelings",
      "Raven matrices",
      "SRI",
      "Digit span",
      "Spatial span",
      "Similarities test",
      # Complex tasks
      "WCST",
      "Reading\ncomprehension"
    )
  } else if (var_selection == "reduced") {
    vars <- c(
      # Reduced variables
      "Visual imagery",
      "Auditory imagery",
      "Sensory imagery",
      "Spatial imagery",
      "Verbal strategies",
      "Raven +\nDigit Span",
      # "Non-verbal\nreasoning",
      "Verbal reasoning",
      "Spatial span std.",
      # Complex tasks
      "WCST",
      "Reading\ncomprehension"
    )
  } else {
    stop("var_selection must be either 'original' or 'reduced'.")
  }
  
  # Filtering the data to keep selected variables only -------------------------
  df_to_plot <- 
    df_long |>
    filter(Variable %in% vars) |> 
    mutate(
      Variable = 
        Variable |> 
        str_replace("Reading\ncomprehension", "Reading") |> 
        str_replace("Psi-Q Sensations", "Psi-Q Sens.") |> 
        str_replace("Psi-Q Feelings", "Psi-Q Feel.") |>  
        str_replace("Audition", "Auditory") |> 
        str_replace("Spatial span std.", "Spatial span") |> 
        factor() |> 
        fct_inorder() |> 
        fct_relevel("Visual imagery", after = Inf) |> 
        fct_relevel("VVIQ", after = Inf),
      Cluster = fct_recode(Cluster, "B (Aphant. + Control)" = "B (Mixed)")
    )
  
  # Writing the formula for the "superb" plot ----------------------------------  
  groups_str <- deparse(substitute(groups))
  if (!(groups_str %in% c("Group", "Cluster", "Subcluster"))) {
    stop("groups must be either 'Group', 'Cluster', or 'Subcluster'.")
  }
  formula    <- as.formula(glue("value ~ Variable + {groups_str}"))
  
  # Choosing the palette based on the group selection --------------------------
  n_groups   <- length(unique(df_long |> pull({{ groups }})))
  
  if (n_groups == 2) {
    palette <- see::okabeito_colors(2, 3)
  } else if (n_groups == 3) {
    palette <- see::okabeito_colors(3, 1, 2)
  } else if (n_groups == 4) {
    palette <- see::okabeito_colors(3, 1, 7, 2)
  } else {
    palette <- see::okabeito_colors()
  }
  palette <- as.character(palette)
  
  # Plotting -------------------------------------------------------------------
  p <- (
    superb(
      formula        = formula,
      data           = df_to_plot,
      plotStyle      = "circularline",
      pointParams    = list(size = dot_size),
      lineParams     = list(linewidth = lw),
      errorbarParams = list(linewidth = lw, show.legend = FALSE),
      adjustments    = list(purpose = "single")
    ) + 
    scale_colour_manual(values = palette) +
    scale_fill_manual(values   = palette) +
    scale_y_continuous(
      limits = c(0, 1),
      breaks = breaks_pretty(),
      expand = expansion(c(0, 0.02))
    ) +
    theme_minimal(base_size = txt_smol) +
    theme(
      # plot wise elements
      plot.title       = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = "grey92"),
      plot.margin      = margin(0, r_off, v_off, l_off, "mm"),
      # legend
      legend.position  = "top",
      legend.title     = element_blank(),
      legend.text      = element_text(size = txt_big),
      legend.key.size  = unit(key, "mm"),
      legend.key.spacing.x = unit(3, "mm"),
      # axes
      axis.line        = element_blank(),
      axis.title.x     = element_blank(),
      axis.title.y     = element_blank(),
      axis.text.y      = element_text(
        size   = txt_smol, 
        margin = margin(0, -y_off, 0, y_off - 4, "mm"),
        hjust  = 0, 
        vjust  = -0.5
      ),
      axis.text.x      = element_text(size = txt_big),
    )
  ) |> suppressMessages()
  
  return(p)
}
