# if (!requireNamespace("pacman")) install.packages("pacman")
pacman::p_load(
  BayesFactor, 
  bayestestR, 
  datawizard, 
  dplyr, 
  emmeans,
  glue,
  logspline,
  modelbased,
  rstanarm,
  tidyr
)

# Model the quantitative variables with the subclusters
model_subclusters <- function(df_long) {
  models <-
    df_long |>
    group_by(Variable) |> 
    nest() |> 
    rowwise() |> 
    mutate(
      stats = list(
        data |> 
          group_by(Subcluster) |> 
          reframe(stats = glue("{round(mean(value), digits = 2)} ({round(sd(value), digits = 2)})")) |> 
          pivot_wider(
            names_from = Subcluster,
            values_from = stats
          )
      ),
      models_inclusion = list(
        generalTestBF(formula = value ~ Subcluster * Age, data = data) |>
          bayesfactor_inclusion() |> 
          rownames_as_column(var = "Variable")
      ),
      models_bf = list(models_inclusion$log_BF),
      models_post = list(
        stan_glm(
          formula = value ~ Subcluster * Age, 
          data = data,
          chains = 4,
          iter   = 10000,
          refresh = 5000
        )
      ),
      contrasts = list(
        estimate_contrasts(
          models_post,
          contrast = "Subcluster",
          test = "bf",
          bf_prior = models_post,
        ) |>
          as.data.frame() |>
          mutate(across(where(is.numeric), ~ round(., digits = 2))) |>
          rename(
            "Subcluster 1" = Level1,
            "Subcluster 2" = Level2
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
      "Subcluster" = models_bf_1,
      Age = models_bf_2,
      "Subcluster $\\times$ Age" = models_bf_3
    ) |> 
    select(!c(data, models_inclusion, models_post))
  
  return(models)
}