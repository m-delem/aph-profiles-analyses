# if (!requireNamespace("pacman")) install.packages("pacman")
pacman::p_load(glue, ggplot2, see, superb)

# Plot two radar plots side by side with the cluster and subcluster results
plot_clusters_radars_2 <- function(
    df, 
    cluster_results,
    txt_big   = 5,
    txt_mid   = 4,
    txt_smol  = 2.5,
    dot_big   = 0.3,
    dot_smol  = 0.1,
    lw_big    = 0.3,
    lw_smol   = 0.1,
    y_off     = 20,
    h_off     = 2,
    v_off     = 0,
    key       = 2,
    ...
    ) {
  
  radar_data_3 <-
    df |> 
    add_clusters(cluster_results) |>
    select(
      contains("cluster"), group,
      vviq:score_comprehension
    ) |>
    # Lines 30-50: dirty hack to align A/B/C cluster names
    mutate(DM_mean   = mean(vviq), DM_n = n(), .by = c(cluster_DM, group)) |> 
    mutate(kPCA_mean = mean(vviq), kPCA_n = n(), .by = c(cluster_kPCA, group)) |> 
    mutate(MDS_mean  = mean(vviq), MDS_n = n(), .by = c(cluster_MDS, group)) |> 
    mutate(
      cluster_DM = case_when(
        DM_mean == max(DM_mean) ~ "A", 
        DM_mean == min(DM_mean) ~ "C", 
        TRUE ~ "B"),
      cluster_kPCA = case_when(
        kPCA_mean == max(kPCA_mean) ~ "A", 
        kPCA_mean == min(kPCA_mean) ~ "C", 
        TRUE ~ "B"),
      cluster_MDS = case_when(
        MDS_mean == max(MDS_mean) ~ "A", 
        MDS_mean == min(MDS_mean) ~ "C", 
        TRUE ~ "B")
    ) |> 
    filter(MDS_n >= 7) |> 
    select(!c(contains("mean"), contains("_n"))) |>
    scale_vars() |> 
    get_long_format() |> 
    get_long_clusters() |> 
    mutate(
      Variable = 
        Variable |> 
        str_replace("Reading\ncomprehension", "Reading") |> 
        str_replace("Psi-Q Sensations", "Psi-Q Sens.") |> 
        factor() |> 
        fct_inorder() |> 
        fct_relevel("VVIQ", after = Inf)
    ) |> 
    select(Group, Cluster, Method, Variable, value)
  
  radar_data_4 <- 
    radar_data_3 |> 
    unite("Cluster", Cluster, Group) |> 
    group_by(Cluster, Method, Variable) |> 
    mutate(n = n()) |>
    ungroup() |>
    mutate(n = paste0("n", n)) |> 
    unite("Cluster", Cluster, n)
  
  plot_radar <- function(radar_data, palette) {
    p <- 
      superb(
        value ~ Variable + Cluster, 
        data = radar_data,
        plotStyle = "circularline",
        pointParams = list(size = dot_smol),
        lineParams = list(linewidth = lw_smol),
        errorbarParams = list(linewidth = lw_smol, show.legend = FALSE),
        adjustments = list(purpose = "single"),
        factorOrder = c("Variable", "Cluster")
      ) + 
      scale_colour_manual(values = palette) +
      scale_fill_manual(values   = palette) +
      scale_y_continuous(
        limits = c(0, 1),
        breaks = breaks_pretty(),
        expand = expansion(c(0, 0.02))
      ) +
      theme_minimal(base_size = txt_mid) +
      theme(
        # plot wise elements
        plot.title = element_text(size = txt_big),
        panel.grid.minor = element_blank(),
        plot.margin      = margin(0, h_off, v_off, h_off, "mm"),
        # legend
        legend.position  = "top",
        legend.title     = element_blank(),
        legend.text      = element_text(size = txt_mid),
        legend.key.size  = unit(key, "mm"),
        # y axis
        axis.text.y      = element_text(
          size = txt_smol, 
          margin = margin(0, -y_off, 0, 0, "mm"),
          hjust = 0, vjust = 0.5
        ),
        axis.line        = element_blank(),
        axis.title.y     = element_blank(),
        # x axis
        axis.text.x      = element_text(size = txt_mid),
        axis.title.x     = element_blank(),
      )
    
    p
  }
  
  pal_3 <- c("#56B4E9", "#E69F00", "#009E73")
  pal_4 <- c("#56B4E9", "#E69F00", "#F5C710", "#009E73")
  
  methods <- unique(radar_data_3$Method)
  
  p3 <- map(
    methods, 
    ~ plot_radar(radar_data_3 |> filter(Method == .x), pal_3) + 
      labs(title = glue("Clustering with {.x} embedding"))
      )
  
  methods <- unique(radar_data_4$Method)
  
  p4 <- map(
    methods, 
    ~ plot_radar(radar_data_4 |> filter(Method == .x), pal_4) + 
      labs(title = glue("Clustering with {.x} embedding")) +
      theme(plot.margin = margin(4, 2, 0, 2, "mm"))
  )
  
  p <- wrap_plots(c(p3, p4), ncol = length(methods), nrows = 2)
  
  return(p)
}