# if (!requireNamespace("pacman")) install.packages("pacman")
pacman::p_load(BayesFactor, dplyr, tidyr)

# Model the association between education, field and occupation with a variable
model_lives <- function(df, groups_var, type = "indepMulti") { # or "jointMulti"
  associations <- 
    df |> 
    pivot_longer(
      c(education, field, occupation), 
      names_to = "Variable", 
      values_to = "value"
    ) |> 
    select({{ groups_var }}, Variable, value) |> 
    group_by(Variable) |> 
    nest() |> 
    rowwise() |> 
    mutate(
      table = list(
        data |> 
          group_by({{ groups_var }}, value) |>
          count() |> 
          pivot_wider(
            names_from = {{ groups_var }},
            values_from = n
          ) |>
          as_tibble() |> 
          mutate(across(!c(value), ~replace_na(.x, 0)))
      ),
      log_bf10 =
        contingencyTableBF(
          as.matrix(table[, names(table) != "value"]), 
          sampleType = type,
          fixedMargin = "cols"
        ) |> 
        as_tibble() |> 
        pull(bf) |> 
        log() |> 
        round(2),
      Variable = str_to_title(Variable)
    )
  
  return(associations)
}