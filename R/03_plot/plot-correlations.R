if (!requireNamespace("pacman")) install.packages("pacman")
pacman::p_load(
  correlation,
  dplyr, 
  here,
  ggplot2, 
  ggraph,
  patchwork,
  see
)
# source(here("R/02_wrangle/get-long-format.R"))
# source(here("R/02_wrangle/transform-vars.R"))
# source(here("R/03_plot/plot-helpers.R"))

plot_correlations <- function(df) {
  correlations <- 
    df |>
    scale_vars() |> 
    select(vviq:score_comprehension) |> 
    correlation(p_adjust = "BY") |> 
    mutate(
      across(
        c(Parameter1, Parameter2),
        ~case_match(
          .x,
          "vviq" ~ "VVIQ",
          "osivq_o" ~ "OSVIQ\nObject",
          "osivq_s" ~ "OSVIQ\nSpatial",
          "osivq_v" ~ "OSVIQ\nVerbal",
          "psiq_vis" ~ "Psi-Q\nVisual",
          "psiq_aud" ~ "Psi-Q\nAudition",
          "psiq_od" ~ "Psi-Q\nSmell",
          "psiq_gout" ~ "Psi-Q\nTaste",
          "psiq_tou" ~ "Psi-Q\nTouch",
          "psiq_sens" ~ "Psi-Q\nSensations",
          "psiq_feel" ~ "Psi-Q\nFeelings",
          "score_raven" ~ "Raven\nMatrices",
          "score_sri" ~ "SRI",
          "span_spatial" ~ "Spatial\nspan",
          "span_digit" ~ "Digit\nspan",
          "wcst_accuracy" ~ "WCST",
          "score_similarities" ~ "Similarities",
          "score_comprehension" ~ "Reading"
        )
      )
    )
  
  correlation_matrix <-
    correlations |> 
    summary(redundant = TRUE) |>
    visualisation_recipe(
      show_data = "points", 
      scale = list(range = c(10, 20)),
      scale_fill = list(
        high = "#009e73", 
        low  = "firebrick2", 
        name = "r"
      )
    ) |> 
    plot() +
    coord_fixed() +
    labs(title = NULL) + 
    theme_modern() + 
    theme(
      legend.position = "none",
      axis.text.x     = element_text(angle = 45, vjust = 1, hjust = 1),
      axis.line       = element_blank()
    )
  
  correlation_graph <-
    correlations |> 
    filter(p < 0.05) |>
    visualisation_recipe() |> 
    plot() +
    scale_edge_colour_gradient2(
      limits = c(-1, 1),
      low    = "firebrick2",
      mid    = "white",
      high   = "#009e73",
      breaks = breaks_pretty(8)
    ) +
    labs(title = NULL) +
    theme(
      legend.frame         = element_rect(),
      legend.justification = c(1, .68),
      legend.key.height    = unit(5.55, "cm"),
      legend.position      = "left",
      legend.text          = element_text(size = 12),
      legend.title         = element_text(size = 22),
      legend.ticks         = element_line(colour = "black"),
    )
  
  # modifying the layers created by visualisation_recipe.easycor_test manually
  correlation_graph$layers[[2]] <- geom_node_point(size = 25)
  correlation_graph$layers[[3]] <- 
    geom_node_text(aes(label = name), colour = "white", size = 3.4)
  
  correlation_joint <-
    (correlation_matrix + correlation_graph) +
    plot_layout(width = c(1, 0.79))
  
  return(correlation_joint)
}