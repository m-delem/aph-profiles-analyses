if (!requireNamespace("pacman")) install.packages("pacman")
pacman::p_load(
  BayesFactor, 
  bayestestR, 
  datawizard, 
  dplyr, 
  emmeans,
  glue,
  modelbased,
  rstanarm,
  tidyr
)

#' Model the quantitative variables with the clusters
#'
#' @param df_clustered The data frame with the clusters (e.g. add_cluster_vars)
#'
#' @details
#' This function return a tibble with many objects in list-columns that 
#' summarise the models fitted on the data:
#' - "data" contains the data subset for each variable
#' - "Cluster X" contain the means and sds for each cluster
#' - "models_inclusion" contains the Bayes factors for the inclusion of each
#'    predictor in the model
#' - "Cluster", "Age", and "Cluster x Age" contain the BFs for each model
#' - "models_post" contains the fitted models
#' - "Difference" and "95% CI" contains the contrasts between the clusters
#' - "log(BF_{10})" contains the Bayes factors for the contrasts
#' 
#' @return A tibble with the results of the models
#' 
model_clusters <- function(df_clustered) {
  cluster_models <- 
    df_clustered |>
    select(
      cluster, age, 
      visual_imagery:span_digit_std, 
      wcst_accuracy, score_comprehension
    ) |>
    pivot_longer(
      !c(cluster, age),
      names_to = "Variable", 
      values_to = "value"
    ) |>
    mutate(
      cluster = fct_relabel(cluster, ~ paste("Cluster", .)),
      Variable = Variable |> 
        str_to_title() |> 
        str_replace_all(c(
          "_" = " ",
          "Non verbal reasoning" = "Non-verbal reasoning",
          "Span spatial std" = "Spatial span",
          "Span digit std" = "Digit span",
          "Wcst accuracy" = "WCST", 
          "Score comprehension" = "Reading comprehension"
        )) |> 
        fct_inorder()
    ) |> 
    rename_with(str_to_title, c(cluster, age)) |> 
    group_by(Variable) |> 
    nest() |> 
    rowwise() |> 
    mutate(
      stats = list(
        data |> 
          group_by(Cluster) |> 
          reframe(stats = glue("{round(mean(value), digits = 2)} ({round(sd(value), digits = 2)})")) |> 
          pivot_wider(
            names_from = Cluster,
            values_from = stats
          )
      ),
      models_inclusion = list(
        generalTestBF(value ~ Cluster * Age, data = data) |>
          bayesfactor_inclusion() |> 
          rownames_as_column(var = "Variable")
      ),
      models_bf = list(models_inclusion$log_BF),
      models_post = list(
        stan_glm(
          value ~ Cluster * Age, 
          data = data,
          chains = 4,
          iter   = 10000,
          refresh = 100
        )
      ),
      contrasts = list(
        estimate_contrasts(
          models_post,
          contrast = "Cluster",
          test = "bf",
          bf_prior = models_post,
          refresh = 100
        ) |> 
          as.data.frame() |> 
          mutate(across(where(is.numeric), ~ round(., digits = 2))) |>
          rename(
            `Cluster 1` = Level1,
            `Cluster 2` = Level2
          ) |>
          rename(`$log(BF_{10})$` = log_BF) |> 
          unite(
            "95% CI",
            c(CI_low, CI_high),
            sep = ", ",
          ) |> 
          mutate(`95% CI` = paste0("[", `95% CI`, "]"))
      )
    ) |> 
    unnest_wider(stats) |> 
    unnest_wider(models_bf, names_sep = "_") |> 
    unnest_longer(contrasts) |>
    unnest_wider(contrasts) |> 
    mutate(across(where(is.numeric), ~ round(., digits = 2))) |> 
    rename(
      Cluster = models_bf_1,
      Age = models_bf_2,
      `Cluster $\\times$ Age` = models_bf_3
    )
  
  return(cluster_models)
}