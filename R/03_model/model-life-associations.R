# if (!requireNamespace("pacman")) install.packages("pacman")
pacman::p_load(BayesFactor, dplyr, tidyr)

# Model the association between education, field and occupation with a variable
model_life_associations <- function(df, clustering, var) {
  associations <- 
    df |> 
    add_cluster_vars(clustering) |> 
    pivot_longer(
      c(education, field, occupation), 
      names_to = "Variable", 
      values_to = "value"
    ) |> 
    select({{ var }}, Variable, value) |> 
    group_by(Variable) |> 
    nest() |> 
    rowwise() |> 
    mutate(
      table = list(
        data |> 
          group_by({{ var }}, value) |>
          count() |> 
          pivot_wider(
            names_from = {{ var }},
            values_from = n
          ) |>
          as_tibble() |> 
          mutate(across(!c(value), ~replace_na(.x, 0)))
      ),
      log_bf10 =
        contingencyTableBF(
          as.matrix(table[, names(table) != "value"]), 
          # sampleType = "jointMulti"
          sampleType = "indepMulti",
          fixedMargin = "cols"
        ) |> 
        as_tibble() |> 
        pull(bf) |> 
        log(),
      Variable = str_to_title(Variable)
    )
}