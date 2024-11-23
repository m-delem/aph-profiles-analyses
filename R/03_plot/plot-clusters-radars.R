# if (!requireNamespace("pacman")) install.packages("pacman")
pacman::p_load(ggplot2, see, superb)

plot_clusters_radars <- function(df, clustering) {
  radar_vars <- c(
    "Cluster"               = "cluster",
    "Cluster"               = "subcluster",
    "Visual\nimagery"       = "visual_imagery", 
    "Sensory\nimagery"      = "sensory_imagery",
    "Spatial\nimagery"      = "spatial_imagery", 
    "Verbal\nstrategies"    = "verbal_strategies",
    "Fluid\nintelligence"   = "fluid_intelligence",
    "Verbal\nreasoning"     = "verbal_reasoning", 
    "Spatial\nspan"         = "spatial_span"
  )
  
  radar_data_3 <-
    df |>
    add_cluster_vars(clustering) |>
    mutate(cluster = case_when(
      cluster == "Cluster A" ~ "A (Control)",
      cluster == "Cluster B" ~ "B (Mixed)",
      cluster == "Cluster C" ~ "C (Aphantasic)",
      TRUE ~ cluster
    )) |>
    select(
      cluster, 
      sensory_imagery:spatial_span, 
      visual_imagery
    ) |> 
    rename(any_of(radar_vars)) |> 
    pivot_longer(
      -Cluster,
      names_to = "Variable", 
      values_to = "value"
    ) |> 
    mutate(Variable = factor(Variable) |> fct_inorder())
  
  radar_data_4 <-
    df |>
    add_cluster_vars(clustering) |>
    select(
      subcluster, 
      sensory_imagery:spatial_span, 
      visual_imagery
    ) |> 
    rename(any_of(radar_vars)) |> 
    pivot_longer(
      -Cluster,
      names_to = "Variable", 
      values_to = "value"
    ) |> 
    mutate(Variable = factor(Variable) |> fct_inorder())
  
  plot_radar <- function(radar_data, palette) {
    p <- 
      superb(
        value ~ Variable + Cluster, 
        data = radar_data,
        plotStyle = "circularline",
        adjustments = list(purpose = "single"),
        factorOrder = c("Variable", "Cluster")
      ) + 
      scale_colour_manual(values = palette) +
      scale_fill_manual(values   = palette) +
      labs(colour = NULL) +
      theme_minimal(base_size = 14) +
      theme(
        panel.grid.minor = element_blank(),
        legend.position = "top",
        legend.text = element_text(size = 14),
        axis.text.y = element_text(
          size = 8, 
          margin = margin(0, -8.4, 0, 0, "cm"),
          hjust = 0, vjust = 0.5
        ),
        axis.line = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_blank(),
        plot.margin = margin(0, 1, 0, 1, "cm")
      )
    
    p
  }
  
  pal_3 <- c("#56B4E9", "#E69F00", "#009E73")
  pal_4 <- c("#56B4E9", "#E69F00", "#F5C710", "#009E73")
  
  p3 <- plot_radar(radar_data_3, pal_3)
  p4 <- plot_radar(radar_data_4, pal_4)
  
  p <- p3 + p4
  
  return(p)
}