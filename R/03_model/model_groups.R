pacman::p_load(logspline)

# Model the quantitative variables with groups, clusters or subclusters
# This functions does a lot of things and gathers all the outputs in a tibble
# 1) It calculates the mean and standard deviation for each group
# 2) It performs a Bayesian model comparison to get BFs for the main effects
# 3) It fits the Bayesian models with rstanarm
# 4) It calculates the contrasts between the groups
# 5) It cleans up the results and returns them in a nice tibble
model_groups <- function(df_long, groups) {
  withr::local_options(list(warn = -1))
  
  groups_str <- deparse(substitute(groups))
  formula <- as.formula(glue::glue("value ~ {groups_str} * Age"))
  
  models <-
    df_long |>
    # Nesting by variable creates sub-dataframes with all the associated data,
    # allowing to perform computations separately for each variable 
    dplyr::group_by(Variable) |> 
    tidyr::nest() |> 
    # The commented lines limit the data to a few variables for tests if needed
    # filter(Variable %in% c(
    #   "VVIQ",
    #   "OSIVQ-Verbal",
    #   "SRI"
    #   )
    # ) |> # for testing
    # 
    # Now we work "rowwise", i.e., for each variable separately, row by row
    dplyr::rowwise() |> 
    dplyr::mutate(
      # 1) Summary statistics for each group -----------------------------------
      stats = list(
        data |>
          dplyr::group_by({{ groups }}) |>
          dplyr::reframe(stats = paste0(
            glue::glue("{round(mean(value), digits = 2)} "),
            glue::glue("({round(sd(value), digits = 2)})")
            )
          ) |>
          tidyr::pivot_wider(
            names_from = {{ groups }},
            values_from = stats
          )
      ),
      # 2a) Bayesian model comparison (gives "ANOVA-like" results) -------------
      model_inclusion = list(
        BayesFactor::generalTestBF(formula = formula, data = data) |>
          bayestestR::bf_inclusion() |>
          datawizard::rownames_as_column(var = "Variable")
      ),
      # 2b) Extracts the Bayes Factors from the previous step
      model_bf = list(model_inclusion$log_BF),
      # 3) Actual Bayesian models with rstanarm, used for contrasts later ------
      model_post = list(
        rstanarm::stan_glm(
          formula = formula,
          data    = data,
          chains  = 4,
          iter    = 10000,
          refresh = 0,
          seed    = 14051998
        )
      ),
      # 4a) Drawing from the prior distribution for contrasts ------------------
      model_prior = list(update(model_post, prior_PD = TRUE)),
      # 4b) Contrasts from the prior and posterior models
      contr_post  = list(
        pairs(emmeans::emmeans(model_post,  specs = groups_str))
      ),
      contr_prior = list(
        pairs(emmeans::emmeans(model_prior, specs = groups_str))
      ),
      # 4c) Extracting the Bayes Factors for the contrasts by comparing the two
      contr_bf = list(
        bayestestR::bf_parameters(contr_post,  prior = contr_prior)
      ),
      # 4d) Extracting the contrasts' dataframe in a nice format
      contrast = list(
        dplyr::left_join(
          contr_post |> tibble::as_tibble() |> dplyr::rename(contrast = 1),
          contr_bf   |> tibble::as_tibble() |> dplyr::rename(contrast = 1),
          by = "contrast"
          ) |>
          dplyr::rename(
            Comparison = contrast,
            `Difference ($\\Delta$)` = estimate,
            `$log(BF_{10})$` = log_BF
            ) |>
          dplyr::mutate(dplyr::across(
            tidyselect::where(is.numeric), 
            ~ round(., digits = 2))
          ) |> 
          tidyr::unite(
            "95% CrI",
            c(lower.HPD, upper.HPD),
            sep = ", "
          ) |>
          dplyr::mutate(`95% CrI` = paste0("[", `95% CrI`, "]"))
      ),
      # `$\\eta^{2}_{p}$` = list(
      #   model_post |> 
      #     eta_squared_posterior() |> 
      #     describe_posterior() |> 
      #     as_tibble() |> 
      #     slice(1) |> 
      #     pull(Median) |> 
      #     round(2)
      # )
    ) |> 
    # 5a) Unnesting the data to get a tibble with all the results side by side -
    tidyr::unnest_wider(stats) |>
    tidyr::unnest_wider(model_bf, names_sep = "_") |>
    tidyr::unnest_longer(contrast) |>
    tidyr::unnest_wider(contrast) |>
    # 5b) Cleaning up with some rounding, renaming and removing useless columns
    dplyr::mutate(dplyr::across(
      tidyselect::where(is.numeric), 
      ~ round(., digits = 2))
    ) |>
    dplyr::rename(
      {{ groups }} := model_bf_1,
      Age          = model_bf_2,
      !!glue::glue("{groups_str} $\\times$ Age") := model_bf_3
    ) |>
    dplyr::select(!c(
      tidyselect::any_of(c("data", "model_inclusion")),
      tidyselect::contains("model_p"),
      tidyselect::contains("contr_")
    ))
  
  return(models)
}
