pacman::p_load(shiny, bslib)

ui <- page_fillable(
  title = "Interactive Exploratory Analyses",
  
  titlePanel("Interactive Exploratory Analyses"),
  
  navset_card_tab(
    
    # Radars -------------------------------------------------------------------
    nav_panel(
      "Variable scores",
        layout_sidebar(
          sidebar = sidebar(
            checkboxGroupInput(
              inputId = "grouping",
              label = "Select grouping variables",
              choices  = c("VVIQ Group" = "Group", "Cluster", "Subcluster"),
              selected = c("Group", "Cluster"),
            ),
            selectInput(
              inputId = "radar_vars", 
              label = "Select a set of variables", 
              choices = c(
                "Original 18 variables" = "original",
                "Reduced 7 variables"   = "reduced"
              ), 
              selected = "original",
              multiple = FALSE
            ),
            sliderInput(
              "sample_size",
              "Sample size",
              min = 60, 
              max = length(unique(df_long$id)), 
              value = length(unique(df_long$id)),
              step = 1
            ),
            sliderInput(
              "txt_size",
              "Text size",
              min = 5,
              max = 14,
              value = 7,
              step = 1
            ),
            sliderInput(
              "element_size",
              "Size of the plot elements",
              min = 0.8,
              max = 4,
              value = 1,
              step = 0.1
            ),
          ),
          
          plotOutput(outputId = "radar")
        )
      ),
    
    # Correlations -------------------------------------------------------------
    nav_panel(
      "Correlations",
      layout_sidebar(
        sidebar = sidebar(
          radioButtons(
            "correlation_type",
            "Select correlation type",
            choices = c("Simple", "Partial"),
            selected = "Simple"
          ),
          checkboxGroupInput(
            "vars",
            "Select variables to plot",
            choices  = unique(corrs_simple$Parameter1),
            selected = unique(corrs_simple$Parameter1)
          ),
          sliderInput(
            "txt_size",
            "Text size",
            min = 5,
            max = 14,
            value = 7,
            step = 1
          ),
        ),
        
        plotOutput(outputId = "correlations")
      )
    ),
    
    # BIC ----------------------------------------------------------------------
    nav_panel(
      "Mixture Models' BICs",
      layout_sidebar(
        sidebar = sidebar(
          sliderInput(
            "sample_size",
            "Sample size",
            min = 60, 
            max = length(unique(df$id)), 
            value = length(unique(df$id)),
            step = 1
          ),
          sliderInput(
            "txt_size",
            "Text size",
            min = 5,
            max = 16,
            value = 10,
            step = 1
          ),
          sliderInput(
            "element_size",
            "Size of the plot elements",
            min = 0.2,
            max = 4,
            value = 0.6
            ,
            step = 0.1
          ),
        ),
        
        plotOutput(outputId = "bic")
      )
    )
  )
)

server <- function(input, output, ...) {
  
  # Radars ---------------------------------------------------------------------
  output$radar <- renderPlot({
    df_filt <- df_long |> filter(id %in% sample(df_long$id, input$sample_size))
    
    p <- list()
    
    for (groups in input$grouping) {
      if (length(p) == 0) {
        p <- plot_radars(
          df_filt, groups = !!groups, var_selection = input$radar_vars,
          txt_big   = input$txt_size + 2, 
          txt_mid   = input$txt_size + 1, 
          txt_small = input$txt_size,
          r_off = input$txt_size,
          l_off = input$txt_size,
          dot_size = input$element_size,
          lw = input$element_size / 3
          )
      } else {
        p <- p + plot_radars(
          df_filt, groups = !!groups, var_selection = input$radar_vars,
          txt_big   = input$txt_size + 2, 
          txt_mid   = input$txt_size + 1, 
          txt_small = input$txt_size,
          r_off = input$txt_size,
          l_off = input$txt_size,
          dot_size = input$element_size,
          lw = input$element_size / 3
          )
      }
    }
    
    print(p)
  })
  
  # Correlations ---------------------------------------------------------------
  output$correlations <- renderPlot({
    corrs <- switch(
      input$correlation_type,
      "Simple"  = corrs_simple,
      "Partial" = corrs_partial
    )
    
    plot_correlations(
      corrs |>
        filter((Parameter1 %in% input$vars) & (Parameter2 %in% input$vars)),
      axis_text = input$txt_size + 1,
      matrix_text = input$txt_size,
      node_text_size = input$txt_size,
      node_size = input$txt_size * 2.6,
      label_text_size = input$txt_size / 2.5
    )
  })
  
  # BIC ------------------------------------------------------------------------
  output$bic <- renderPlot({
    # variables selected for clustering after the analysis of correlations
    selected_vars <- c(
      "visual_imagery", "sensory_imagery", 
      "spatial_imagery", "verbal_strategies",
      "fluid_intelligence", "verbal_reasoning", 
      "span_spatial"
    )
    
    # determine the best model (GMM) and number of clusters for clustering with
    clustering <- 
      df |> 
      slice(1:input$sample_size) |> 
      scale_reduce_vars() |> 
      select(any_of(selected_vars)) |>
      Mclust(verbose = FALSE)
    
    plot_clusters_bic(
      clustering,
      txt_big  = input$txt_size + 2,
      txt_mid  = input$txt_size + 1,
      txt_smol = input$txt_size,
      size = input$element_size
      )
  })
}

shinyApp(ui, server)