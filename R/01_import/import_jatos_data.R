#' Import, tidy and save JATOS data
#' 
#' @description 
#' This function imports the raw data from JATOS, tidies it up, and saves it in
#' various formats (Excel, CSV, RDS).
#'  
#' @return A list containing the final data frame and the metadata.
import_jatos_data <- function() {
  
  # Retrieving metadata --------------------------------------------------------
  
  df_meta <- 
    readxl::read_xlsx(here::here("data/data-raw/metadata.xlsx")) |> 
    dplyr::mutate(
      Duration = Duration |> 
        format(
          format = "%H:%M:%S",
          digits = 0
        ) |> 
        as.difftime(),
      `Start Time` = as.character(`Start Time`),
      `Last Seen` = as.character(`Last Seen`)
    )
  
  
  # Extracting and tidying raw data --------------------------------------------
  
  df <-
    tibble::tibble(path = fs::dir_ls(
      path = "data/data-raw", 
      regexp = ".txt", 
      recurse = TRUE
    )) |> 
    dplyr::rowwise() |> 
    dplyr::mutate(data = list(jsonlite::read_json(here::here(path)))) |> 
    tidyr::unnest_longer(data) |> 
    # splitting the path into columns
    tidyr::separate_wider_delim(
      path, 
      "/", 
      names = c(
        "nope_1", 
        "nope_2", 
        "subject_id", 
        "comp_id", 
        "nope_3"
      )
    ) |> 
    # simplifying participant column
    tidyr::separate_wider_delim(
      subject_id, 
      "_", 
      names = c("nope_4", "nope_5", "id")
    ) |> 
    # simplifying component column
    tidyr::separate_wider_delim(
      comp_id, 
      stringr::regex("[_-]"), 
      names = c("nope_6", "nope_7", "comp_id")
    ) |> 
    # deleting useless columns
    dplyr::select(!tidyselect::starts_with("nope"))
  
  
  # Unfolding span data --------------------------------------------------------
  
  df_spans <-
    df |>
    # keeping only spans
    dplyr::filter(data_id == "") |>
    # keeping only responses
    dplyr::rowwise() |> 
    dplyr::filter("response" %in% data) |> 
    dplyr::select(-data_id) |> 
    tidyr::unnest_wider(data) |> 
    # deleting useless columns
    dplyr::select(-c(2, 4:10, 12, 13, 17, 20, 21)) |> 
    # unlisting and comparing sequences
    dplyr::rowwise() |> 
    dplyr::mutate(
      response = list(unlist(response)),
      sequence = list(unlist(sequence)),
      correct_seq = list(rev(sequence)),
      # shortening responses to sequence length max
      response = list(response[1:length(correct_seq)]),
      # cleaning NAs
      response = list(ifelse(is.na(response), 0, response)),
      # counting correct responses at the correct place
      num_true = sum(response == correct_seq),
      # merging spaces and digits numbers columns
      num_spaces = ifelse(is.na(num_spaces), num_digits, num_spaces)
    ) |> 
    dplyr::rename(num_items = num_spaces) |> 
    # calculating averages
    dplyr::group_by(id, exp_id) |> 
    dplyr::mutate(avg_span = mean(num_true)) |> 
    # deleting useless columns
    dplyr::select(-c(rt, response, sequence, num_digits, correct_seq))
  
  
  # Unfolding WCST data --------------------------------------------------------
  
  df_wcst <- 
    df |> 
    # filtering out empty cells
    dplyr::filter(
      data_id != "" & !stringr::str_detect(data_id,"fig|art|page")
    ) |> 
    dplyr::select(-comp_id) |> 
    # deploying all variables
    tidyr::pivot_wider(
      names_from = data_id,
      values_from = data
    ) |>
    dplyr::rename(wcst = data) |> 
    dplyr::select(id, wcst) |> 
    tidyr::unnest_longer(wcst) |> 
    tidyr::unnest_wider(wcst) |>
    dplyr::select(id, accuracy, average_response_time) |> 
    dplyr::rename(
      "wcst_accuracy" = accuracy,
      "wcst_rt_avg"  = average_response_time) |> 
    dplyr::group_by(id) |> 
    dplyr::filter(dplyr::row_number() == dplyr::n())
  
  # Raw questionnaire responses ------------------------------------------------
  
  df_questionnaires <-
    df |> 
    dplyr::filter(stringr::str_detect(
      data_id, 
      paste0(
        "(age|sexe|education|vviq|osviq|vis_|aud_|od_|gout_|tou_|sens_|feel_",
        "|raven|sri)",
        "(?!.*Comment)"
      )
    )) |> 
    dplyr::select(!comp_id) |>
    # deploying all variables
    tidyr::pivot_wider(
      names_from = data_id,
      values_from = data
    ) |>  
    dplyr::rename(sex = sexe) |> 
    dplyr::select(!c(
      tidyselect::contains("page"), 
      tidyselect::contains("rt"))
    ) |> 
    tidyr::unnest_wider(c(vviq, osviq)) |>
    dplyr::mutate(dplyr::across(
      c(
        tidyselect::contains("vviq"), 
        tidyselect::contains("osviq"), 
        vis_1:feel_3
      ), 
      as.numeric)
    ) |> 
    dplyr::rename_with(
      ~ stringr::str_replace(., "osviq_", "osivq_"),
      tidyselect::starts_with("osviq")
    ) |> 
    dplyr::rename_with(
      ~ paste0("psiq_", .),
      c(vis_1:feel_3)
    )
  
  
  # Isolating manual scoring data ----------------------------------------------
  
  df_similarities <-
    df |> 
    dplyr::filter(stringr::str_detect(data_id, "simili")) |> 
    # deploying all variables
    tidyr::pivot_wider(
      names_from = data_id,
      values_from = data
    ) |> 
    dplyr::select(!c(id, comp_id))
  
  df_comprehension <-
    df |> 
    dplyr::filter(stringr::str_detect(data_id, "question")) |> 
    # deploying all variables
    tidyr::pivot_wider(
      names_from = data_id,
      values_from = data
    ) |> 
    dplyr::select(!c(id, comp_id, question1, question2, question3))
  
  
  # Retrieving manually scored data --------------------------------------------
  
  df_scored_manually <- readxl::read_xlsx(
    here::here("data/data-processed/data_scored_manually.xlsx")
    )
  
  
  # Merginig all scores and classifications ------------------------------------
  
  sum_items <- function(name){
    rowSums(dplyr::across(tidyselect::starts_with(name)), na.rm = TRUE)
  }
  
  df_final <-
    df_questionnaires |> 
    dplyr::left_join(
      df_wcst,
      by = "id"
    ) |> 
    dplyr::left_join(
      df_spans |> 
        dplyr::select(id, exp_id, avg_span) |>
        dplyr::distinct() |>
        tidyr::pivot_wider(
          names_from = exp_id,
          values_from = avg_span
        ),
      by = "id"
    ) |>
    dplyr::left_join(
      df_scored_manually,
      by = "id"
    ) |>
    dplyr::rename(
      span_spatial = `spatial-span`,
      span_digit = `digit-span`
    ) |> 
    dplyr::rowwise() |>
    dplyr::mutate(
      # reverting inverted items
      osivq_v_2  = 6 - osivq_v_2,
      osivq_v_9  = 6 - osivq_v_9,
      osivq_v_41 = 6 - osivq_v_41,
      osivq_s_42 = 6 - osivq_s_42
    ) |> 
    dplyr::mutate(
      # cumulative scores by scale
      vviq = sum_items("vviq_"),
      osivq_o = sum_items("osivq_o_"),
      osivq_s = sum_items("osivq_s_"),
      osivq_v = sum_items("osivq_v_"),
      psiq_vis  = round((sum_items("psiq_vis")/3), digits = 2),
      psiq_aud  = round((sum_items("psiq_aud")/3), digits = 2),
      psiq_od   = round((sum_items("psiq_od")/3),  digits = 2),
      psiq_gout = round((sum_items("psiq_gou")/3), digits = 2),
      psiq_tou  = round((sum_items("psiq_tou")/3), digits = 2),
      psiq_sens = round((sum_items("psiq_sen")/3), digits = 2),
      psiq_feel = round((sum_items("psiq_fee")/3), digits = 2),
      .keep = "unused"
    ) |> 
    dplyr::mutate(
      # grouping by VVIQ according to convention
      group = ifelse(vviq <= 32, "Aphantasic", "Control"),
      group = factor(group, levels = c("Control", "Aphantasic")),
      
      # education levels have been coded by adapting the French grades
      # to the International Standard Classification of Education (ISCED)
      education = dplyr::case_match(
        education, 
        "other"  ~ "Other",
        "brevet" ~  "Upper secondary",
        "bac" ~ "Post-secondary",
        "licence" ~ "Bachelor",
        "master" ~ "Master",
        "doctorat" ~ "Doctorate",
        .ptype = factor(levels = c(
          "Other", 
          "Upper secondary", 
          "Post-secondary", 
          "Bachelor", 
          "Master", 
          "Doctorate"
        ))
      ),
      dplyr::across(tidyselect::contains("_code"), as.numeric),
      # Fields of education have already been coded according to the 10 broad 
      # fields defined by the ISCED-F 2013
      # Occupations have already been coded according to the International 
      # Standard Classification of Occupations (ISCO-08)
      # I'll recode from 1 to 9 for the sake of clarity
      occupation_code = dplyr::case_match(
        occupation_code,
        0  ~ 1,
        1  ~ 2,
        2  ~ 3,
        21 ~ 4,
        22 ~ 5,
        23 ~ 6,
        24 ~ 7,
        25 ~ 8,
        26 ~ 9,
      )
    ) |>
    # Reordering field and occupation categories
    dplyr::arrange(field_code) |> 
    dplyr::mutate(field = forcats::fct_reorder(field, field_code)) |>
    dplyr::arrange(occupation_code) |>
    dplyr::mutate(
      occupation = forcats::fct_reorder(occupation, occupation_code),
      field_code = factor(field_code, levels = seq(0, max(field_code))),
      occupation_code = factor(occupation_code)
    ) |>
    # Back to sorting by id
    dplyr::arrange(id) |>
    dplyr::select(
      id, age, sex, group,
      education, field, field_code,
      occupation, occupation_code,
      vviq, osivq_o, osivq_s, osivq_v, 
      tidyselect::starts_with("psiq"), 
      score_raven, score_sri,
      span_spatial, span_digit,
      wcst_accuracy,
      score_similarities, score_comprehension
    ) |> 
    dplyr::mutate(
      dplyr::across(c(sex:field,occupation), as.factor),
      dplyr::across(c(age, vviq:score_comprehension), as.numeric),
      dplyr::across(tidyselect::where(is.numeric), ~ round(., 2))
    ) |> 
    dplyr::ungroup()
  
  
  # Exporting in various formats ------------------------------
  
  # Excel, all clean data
  openxlsx::write.xlsx(
    list(
      "data_final"  = df_final,
      "similarities" = df_similarities,
      "comprehension" = df_comprehension,
      "metadata" = df_meta
    ),
    here::here("data/data-processed/data_tidied.xlsx"),
    asTable = TRUE,
    colNames = TRUE,
    colWidths = "auto",
    borders = "all",
    tableStyle = "TableStyleMedium16"
  )
  
  # CSV, main data only
  readr::write_csv(df_final, here::here("data/data-processed/data_tidied.csv"))
  
  # RDS, main data with correct variable types
  saveRDS(df_final, here::here("data/data-processed/data_tidied.rds"))
  
  
  data_list <-
    list(
      "data_final"  = df_final,
      "metadata" = df_meta
    )
  
  return(data_list)
}