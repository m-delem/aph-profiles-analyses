# if (!requireNamespace("pacman")) install.packages("pacman")
pacman::p_load(
  BayesFactor, # generalTestBF
  bayestestR,  # bf_inclusion, bf_parameters
  datawizard,  # rownames_as_column
  dplyr, 
  emmeans,     # pairs, emmeans
  glue,
  logspline,
  rstanarm,    # stan_glm
  tidyr
)

# Model the quantitative variables with the VVIQ groups
model_groups <- function(df_long, groups) {
  withr::local_options(list(warn = -1))
  
  groups_str <- deparse(substitute(groups))
  formula <- as.formula(glue("value ~ {groups_str} * Age"))
  
  models <-
    df_long |>
    group_by(Variable) |> 
    nest() |> 
    # filter(Variable %in% c(
    #   "VVIQ",
    #   # "OSIVQ-Spatial",
    #   "OSIVQ-Verbal",
    #   # "Raven matrices",
    #   "SRI"
    #   )
    # ) |> # for testing
    rowwise() |> 
    mutate(
      stats = list(
        data |>
          group_by({{ groups }}) |>
          reframe(stats = paste0(
            glue("{round(mean(value), digits = 2)} "),
            glue("({round(sd(value), digits = 2)})")
            )
          ) |>
          pivot_wider(
            names_from = {{ groups }},
            values_from = stats
          )
      ),
      model_inclusion = list(
        generalTestBF(formula = formula, data = data) |>
          bf_inclusion() |>
          rownames_as_column(var = "Variable")
      ),
      model_bf = list(model_inclusion$log_BF),
      model_post = list(
        stan_glm(
          formula = formula,
          data    = data,
          chains  = 4,
          iter    = 10000,
          refresh = 0,
          seed    = 14051998
        )
      ),
      model_prior = list(update(model_post, prior_PD = TRUE)),
      contr_post  = list(pairs(emmeans(model_post,  specs = groups_str))),
      contr_prior = list(pairs(emmeans(model_prior, specs = groups_str))),
      contr_bf    = list(bf_parameters(contr_post,  prior = contr_prior)),
      contrast = list(
        left_join(
          contr_post |> as_tibble() |> rename(contrast = 1),
          contr_bf   |> as_tibble() |> rename(contrast = 1),
          by = "contrast"
        ) |>
          rename(
            Comparison = contrast,
            `Difference ($\\Delta$)` = estimate,
            `$log(BF_{10})$` = log_BF
            ) |>
          mutate(across(where(is.numeric), ~ round(., digits = 2))) |> 
          unite(
            "95% CrI",
            c(lower.HPD, upper.HPD),
            sep = ", "
          ) |>
          mutate(`95% CrI` = paste0("[", `95% CrI`, "]"))
      )
    ) |> 
    unnest_wider(stats) |>
    unnest_wider(model_bf, names_sep = "_") |>
    unnest_longer(contrast) |>
    unnest_wider(contrast) |>
    mutate(across(where(is.numeric), ~ round(., digits = 2))) |>
    rename(
      {{ groups }} := model_bf_1,
      Age             = model_bf_2,
      !!glue("{groups_str} $\\times$ Age") := model_bf_3
    ) |>
    select(!c(
      any_of(c("data", "model_inclusion")), 
      contains("model_p"),
      contains("contr_")
    ))
  
  return(models)
}
