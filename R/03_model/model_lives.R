# Model the association between education, field and occupation with a variable
model_lives <- function(df, groups_var, type = "indepMulti") { # or "jointMulti"
  associations <- 
    df |> 
    tidyr::pivot_longer(
      c(education, field, occupation), 
      names_to = "Variable", 
      values_to = "value"
    ) |> 
    dplyr::select({{ groups_var }}, Variable, value) |> 
    dplyr::group_by(Variable) |> 
    tidyr::nest() |> 
    dplyr::rowwise() |> 
    dplyr::mutate(
      table = list(
        data |> 
          dplyr::group_by({{ groups_var }}, value) |>
          dplyr::count() |> 
          tidyr::pivot_wider(
            names_from = {{ groups_var }},
            values_from = n
          ) |>
          tibble::as_tibble() |> 
          dplyr::mutate(dplyr::across(!c(value), ~tidyr::replace_na(., 0)))
      ),
      log_bf10 =
        BayesFactor::contingencyTableBF(
          as.matrix(table[, names(table) != "value"]), 
          sampleType = type,
          fixedMargin = "cols"
        ) |> 
        tibble::as_tibble() |> 
        dplyr::pull(bf) |> 
        log() |> 
        round(2),
      Variable = stringr::str_to_title(Variable)
    )
  
  return(associations)
}

# Display the distribution of the sample on a variable for a classification
show_lives <- function(lives_table, variable) {
  lives_table |>
    dplyr::filter(Variable == variable) |> 
    tidyr::unnest(table) |> 
    dplyr::ungroup() |> 
    dplyr::select(!c(Variable, data, log_bf10)) |> 
    dplyr::rename(!!variable := value) |> 
    knitr::kable()
}
