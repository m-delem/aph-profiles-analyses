# if (!requireNamespace("pacman")) install.packages("pacman")
pacman::p_load(
  dplyr, 
  here,
  forcats,
  fs,
  jsonlite,
  lubridate, 
  openxlsx,
  purrr, 
  readr,
  readxl, 
  stringr, 
  tidyr
)

# Import, tidy and save JATOS data
import_jatos_data <- function() {
  
  # Retrieving metadata --------------------------------------------------------
  
  df_meta <- 
    read_xlsx(here("data/data-raw/metadata.xlsx")) |> 
    mutate(
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
    tibble(path = dir_ls(
      path = here("data/data-raw"), 
      regexp = ".txt", 
      recurse = TRUE
    )) |> 
    rowwise() |> 
    mutate(data = list(read_json(path))) |> 
    unnest_longer(data) |> 
    # splitting the path into columns
    separate_wider_delim(
      path, 
      "/", 
      names = c(
        "nope_1", 
        "nope_2", 
        "nope_3", 
        "nope_4", 
        "nope_5", 
        "nope_6", 
        "nope_7", 
        "nope_8", 
        "subject_id", 
        "comp_id", 
        "nope_9"
      )
    ) |> 
    # simplifying participant column
    separate_wider_delim(
      subject_id, 
      "_", 
      names = c("nope_10", "nope_11", "id")
    ) |> 
    # simplifying component column
    separate_wider_delim(
      comp_id, 
      regex("[_-]"), 
      names = c("nope_12", "nope_13", "comp_id")
    ) |> 
    # deleting useless columns
    select(!starts_with("nope"))
  
  
  # Unfolding span data --------------------------------------------------------
  
  df_spans <-
    df |>
    # keeping only spans
    filter(data_id == "") |>
    # keeping only responses
    rowwise() |> 
    filter("response" %in% data) |> 
    select(-data_id) |> 
    unnest_wider(data) |> 
    # deleting useless columns
    select(-c(2, 4:10, 12, 13, 17, 20, 21)) |> 
    # unlisting and comparing sequences
    rowwise() |> 
    mutate(
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
    rename(num_items = num_spaces) |> 
    # calculating averages
    group_by(id, exp_id) |> 
    mutate(avg_span = mean(num_true)) |> 
    # deleting useless columns
    select(-c(rt, response, sequence, num_digits, correct_seq))
  
  
  # Unfolding WCST data --------------------------------------------------------
  
  df_wcst <- 
    df |> 
    # filtering out empty cells
    filter(data_id != "" & !str_detect(data_id,"fig|art|page")) |> 
    select(-comp_id) |> 
    # deploying all variables
    pivot_wider(
      names_from = data_id,
      values_from = data
    ) |>
    rename(wcst = data) |> 
    select(id, wcst) |> 
    unnest_longer(wcst) |> 
    unnest_wider(wcst) |>
    select(id, accuracy, average_response_time) |> 
    rename(
      "wcst_accuracy" = accuracy,
      "wcst_rt_avg"  = average_response_time) |> 
    group_by(id) |> 
    filter(row_number() == n())
  
  # Raw questionnaire responses ------------------------------------------------
  
  df_questionnaires <-
    df |> 
    filter(str_detect(
      data_id, 
      paste0(
        "(age|sexe|education|vviq|osviq|vis_|aud_|od_|gout_|tou_|sens_|feel_",
        "|raven|sri)",
        "(?!.*Comment)"
      )
    )) |> 
    select(!comp_id) |>
    # deploying all variables
    pivot_wider(
      names_from = data_id,
      values_from = data
    ) |>  
    rename(sex = sexe) |> 
    select(!c(contains("page"), contains("rt"))) |> 
    unnest_wider(c(vviq, osviq)) |>
    mutate(across(
      c(contains("vviq"), contains("osviq"), vis_1:feel_3), 
      as.numeric)
    ) |> 
    rename_with(
      ~ str_replace(., "osviq_", "osivq_"),
      starts_with("osviq")
    ) |> 
    rename_with(
      ~ paste0("psiq_", .),
      c(vis_1:feel_3)
    )
  
  
  # Isolating manual scoring data ----------------------------------------------
  
  df_similarities <-
    df |> 
    filter(str_detect(data_id, "simili")) |> 
    # deploying all variables
    pivot_wider(
      names_from = data_id,
      values_from = data
    ) |> 
    select(!c(id, comp_id))
  
  df_comprehension <-
    df |> 
    filter(str_detect(data_id, "question")) |> 
    # deploying all variables
    pivot_wider(
      names_from = data_id,
      values_from = data
    ) |> 
    select(!c(id, comp_id, question1, question2, question3))
  
  
  # Retrieving manually scored data --------------------------------------------
  
  df_scored_manually <- read_xlsx(here("data/data-processed/data_scored_manually.xlsx"))
  
  
  # Merginig all scores and classifications ------------------------------------
  
  sum_items <- function(name){rowSums(across(starts_with(name)), na.rm = TRUE)}
  
  df_final <-
    df_questionnaires |> 
    left_join(
      df_wcst,
      by = "id"
    ) |> 
    left_join(
      df_spans |> 
        select(id, exp_id, avg_span) |>
        distinct() |>
        pivot_wider(
          names_from = exp_id,
          values_from = avg_span
        ),
      by = "id"
    ) |>
    left_join(
      df_scored_manually,
      by = "id"
    ) |>
    rename(
      span_spatial = `spatial-span`,
      span_digit = `digit-span`
    ) |> 
    rowwise() |>
    mutate(
      # reverting inverted items
      osivq_v_2  = 6 - osivq_v_2,
      osivq_v_9  = 6 - osivq_v_9,
      osivq_v_41 = 6 - osivq_v_41,
      osivq_s_42 = 6 - osivq_s_42
    ) |> 
    mutate(
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
    mutate(
      # grouping by VVIQ according to convention
      group = ifelse(vviq <= 32, "Aphantasic", "Control"),
      group = factor(group, levels = c("Control", "Aphantasic")),
      
      # education levels have been coded by adapting the French grades
      # to the International Standard Classification of Education (ISCED)
      education = case_match(
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
      across(contains("_code"), as.numeric),
      # Fields of education have already been coded according to the 10 broad 
      # fields defined by the ISCED-F 2013
      # Occupations have already been coded according to the International 
      # Standard Classification of Occupations (ISCO-08)
      # I'll recode from 1 to 9 for the sake of clarity
      occupation_code = case_match(
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
    arrange(field_code) |> 
    mutate(field = fct_reorder(field, field_code)) |>
    arrange(occupation_code) |>
    mutate(
      occupation = fct_reorder(occupation, occupation_code),
      field_code = factor(field_code, levels = seq(0, max(field_code))),
      occupation_code = factor(occupation_code)
    ) |>
    # Back to sorting by id
    arrange(id) |>
    select(
      id, age, sex, group,
      education, field, field_code,
      occupation, occupation_code,
      vviq, osivq_o, osivq_s, osivq_v, 
      starts_with("psiq"), 
      score_raven, score_sri,
      span_spatial, span_digit,
      wcst_accuracy,
      score_similarities, score_comprehension
    ) |> 
    mutate(
      across(c(sex:field,occupation), as.factor),
      across(c(age, vviq:score_comprehension), as.numeric),
      across(where(is.numeric), ~ round(., 2))
    ) |> 
    ungroup()
  
  
  # Exporting in various formats ------------------------------
  
  # Excel, all clean data
  write.xlsx(
    list(
      "data_final"  = df_final,
      "similarities" = df_similarities,
      "comprehension" = df_comprehension,
      "metadata" = df_meta
    ),
    here("data/data-processed/data_tidied.xlsx"),
    asTable = TRUE,
    colNames = TRUE,
    colWidths = "auto",
    borders = "all",
    tableStyle = "TableStyleMedium16"
  )
  
  # CSV, main data only
  write_csv(df_final, here("data/data-processed/data_tidied.csv"))
  
  # RDS, main data with correct variable types
  saveRDS(df_final, here("data/data-processed/data_tidied.rds"))
  
  
  data_list <-
    list(
      "data_final"  = df_final,
      "metadata" = df_meta
    )
  
  return(data_list)
}