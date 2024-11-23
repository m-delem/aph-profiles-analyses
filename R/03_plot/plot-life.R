pacman::p_load(dplyr, forcats, patchwork)

# Plot education, field and occupation by group, cluster or subcluster
plot_life <- function(
    df, clustering, group_var,
    bar_alp = 0.5,
    bar_wid = 0.75,
    widths  = c(1, 1.6, 1.25)
) {
  choice <- deparse(substitute(group_var))
  
  if (choice == "group") {
    pal <- c("#56B4E9", "#009E73")
  } else if (choice == "cluster") {
    pal <- c("#56B4E9", "#E69F00", "#009E73")
  } else if (choice == "subcluster"){
    pal <- c("#56B4E9", "#E69F00", "#F5C710", "#009E73")
  } else {
    stop(
      "Invalid group variable. ",
      "Choose between 'group', 'cluster', or 'subcluster'.")
  }
  
  df <- 
    df |>
    mutate(
      field = case_when(
        field_code == "3" ~ "Social sciences",
        field_code == "4" ~ "Business",
        field_code == "5" ~ "Natural sciences",
        field_code == "6" ~ "Communication",
        field_code == "7" ~ "Engineering",
        field_code == "8" ~ "Agriculture",
        TRUE ~ field
      ) |> factor() |> fct_reorder(as.numeric(field_code)),
      occupation = case_when(
        occupation_code == "4" ~ "Science",
        occupation_code == "7" ~ "Business",
        occupation_code == "8" ~ "Communication",
        occupation_code == "9" ~ "Social",
        TRUE ~ occupation
      ) |> factor() |> fct_reorder(as.numeric(occupation_code))
    )
  
  plot_life_var <- function(df, life_var) {
    p <- 
      df |>  
      add_cluster_vars(clustering) |>
      count({{ group_var }}, {{ life_var }}) |> 
      group_by({{ group_var }}) |>
      mutate(
        sum  = sum(n),
        prop = (n / sum)*100
      ) |> 
      ggplot() +
      geom_col(
        aes(
          x     = {{ life_var }}, 
          y     = prop,
          fill  = {{ group_var }},
          color = {{ group_var }}
        ),
        position = "dodge",
        alpha    = bar_alp,
        width    = bar_wid
      ) +
      scale_y_continuous(breaks = seq(0, 100, 5), expand = expansion(c(0, 0.1))) +
      scale_fill_manual(values = pal) +
      scale_color_manual(values = pal) +
      labs(y = "Proportion (%)") +
      theme_modern(base_size = 14) +
      theme(
        legend.position = "top",
        legend.text     = element_text(size = 16),
        legend.title    = element_blank(),
        panel.grid.major.y = element_line(),
        panel.grid.minor.y = element_line(),
        axis.title.y    = element_text(size = 16),
        axis.title.x    = element_text(size = 16),
        # axis.title.x    = element_blank(),
        axis.ticks.x    = element_line(),
        axis.text.x     = element_text(angle = 45, hjust = 1, size = 12)
      )
    
    return(p)
  }
  
  p1 <- plot_life_var(df, education) + xlab("Education level")
  p2 <- plot_life_var(df, field) + 
    xlab("Field of study") +
    theme(axis.title.y = element_blank())
  p3 <- plot_life_var(df, occupation) + 
    xlab("Occupation") +
    theme(axis.title.y = element_blank())
  
  p <- p1 + p2 + p3 + plot_layout(guides = "collect", widths = widths) &
    theme(legend.position = "top")
  
  return(p)
}
