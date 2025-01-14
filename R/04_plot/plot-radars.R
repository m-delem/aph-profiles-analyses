# if (!requireNamespace("pacman")) install.packages("pacman")
pacman::p_load(
  dplyr, 
  forcats, 
  glue, 
  ggplot2, 
  # ggtext, 
  patchwork,
  see, 
  stringr, 
  superb, 
  withr
  )

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
      "Sensory imagery",
      "Spatial imagery",
      "Verbal strategies",
      "Raven +\nDigit Span",
      # "Non-verbal\nreasoning",
      "Verbal reasoning",
      "Spatial span std.",
      # Complex tasks
      "WCST",
      "Reading\ncomprehension",
      "Auditory imagery"
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
        # The three lines below were ggtext attempts to put selected labels in 
        # italics using markdown.
        # str_replace("Reading\ncomprehension", "*Reading*") |>
        # str_replace("Auditory imagery", "*Auditory imagery*") |>
        # str_replace("WCST", "*WCST*") |>
        str_replace("Psi-Q Sensations", "Psi-Q Sens.") |> 
        str_replace("Psi-Q Feelings", "Psi-Q Feel.") |>  
        str_replace("Audition", "Auditory") |> 
        str_replace("Spatial span std.", "Spatial span") |> 
        factor() |> 
        fct_inorder() |> 
        fct_relevel("Auditory imagery", after = Inf) |>
        # fct_relevel("*Auditory imagery*", after = Inf) |> # in ggtext version
        fct_relevel("Visual imagery", after = Inf) |> 
        fct_relevel("VVIQ", after = Inf),
      Cluster = fct_recode(Cluster, "B (Aphant. + Control)" = "B (Mixed)")
    )
  
  # The lines below were an alternative clunky attempt to italicize selected 
  # labels. We create an expression() vector to be used in scale_x_discrete.
  # It worked for the italics, but the scale_x_discrete broke the radars by
  # adding space between the first and last breaks.

  label <- df_to_plot$Variable |> levels()
  label <- replace(label, label == "Reading", expression(~italic("Reading")))
  label <- replace(
    label,
    label == "Auditory imagery",
    expression(~italic("Auditory imagery")))
  label <- replace(label, label == "WCST", expression(~italic("WCST")))
  
  # Writing the formula for the "superb" plot ----------------------------------
  
  # Below are various previous attempts to allow the "groups" argument to be
  # either a string or a symbol. The current version uses rlang::ensym(groups).
  
  # groups_str <- deparse(substitute(groups))
  # groups_str <- as.character(substitute(groups))
  
  # if (is.character(groups)) {
  #   groups_str <- groups
  #   } else {
  #   groups_str <- deparse(substitute(groups))
  #   }
  
  # subs <- substitute(groups)
  # groups_str <- if (is.character(subs)) subs else deparse(subs)
  
  # groups_str <- deparse(rlang::ensym(groups))
  
  # This is the version that finally worked:
  groups_str <- as.character(rlang::ensym(groups))

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
    palette <- see::okabeito_colors(3, 9, 6, 2)
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
    # Below is the clunky attempt to italicize selected labels.
    # It worked for the italics, but broke the radars.
    # scale_x_discrete(
    #   expand = expansion(add = 0, mult = 0),
    #   limits = factor(df_to_plot$Variable |> levels()),
    #   labels = label
    # ) +
    # coord_polar(start = pi/36) +
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
      # Below is the ggtext attempt to italicize selected labels. 
      # Once again, the italic worked, but this time ggtext broke the patchwork 
      # layouts, creating the error: 
      #   Error in grid.Call.graphics(C_setviewport, vp, TRUE)
      # From what I read in a GH issue, it seems to be a ggtext bug.
      # Sad. I was close to the solution.
      # axis.text.x      = element_markdown(
      #   size    = txt_big, 
      #   margin  = unit(c(0, 0, 0, 0), "pt"),
      #   padding = unit(c(0, 0, 0, 0), "pt"),
      # ),
      axis.text.x      = element_text(size = txt_big),
      axis.text.y      = element_text(
        size   = txt_smol, 
        margin = margin(0, -y_off, 0, y_off - 4, "mm"),
        hjust  = 0, 
        vjust  = -0.5
      ),
    )
  ) |> suppressMessages()
  
  return(p)
}
