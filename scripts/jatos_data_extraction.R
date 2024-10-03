library(here)
source(here("scripts/_setup.R"))


# Extracting and tidying raw data ----------------------------------------------

df <-
  # listing all folders in the data folder
  dir_ls(path = "data/data-raw", regexp = "study") |> 
  # transforming it into a character vector
  as.character() |> 
  # listing all subfolders in each folder...
  dir_ls() |> 
  # ... and the datafile in each subfolder.
  map_chr(dir_ls) |> 
  # converting into a tibble
  tibble() |> 
  # renaming the ugly column
  rename(path = "map_chr(...)") |> 
  # reading the .txt as JSON
  rowwise() |> 
  mutate(data = list(map(path, read_json))) |> 
  # detailing the columns
  separate_wider_delim(
    path, 
    "/", 
    names = c(
      "folder_1", 
      "folder_2", 
      "participant_id", 
      "component_id", 
      "json"
    )
  ) |> 
  # pulling out data
  unnest_longer(data) |> 
  # deleting useless columns
  select(-c(starts_with("folder"), json, data_id)) |> 
  # pulling out individual results again
  unnest_longer(data) |>
  # renaming participant column
  separate_wider_delim(
    participant_id, 
    "_", 
    names = c(
      "study", 
      "result", 
      "id"
    )
  ) |> 
  select(-c(study, result))


# Unfolding span data ----------------------------------------------------------

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


# Unfolding WCST data ----------------------------------------------------------

df_wcst <- 
  df |> 
  # filtering out empty cells
  filter(data_id != "" & data != "NULL") |> 
  select(-component_id) |> 
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


# Raw questionnaire responses --------------------------------------------------

df_questionnaires <-
  df |> 
  # filtering out empty cells
  filter(data_id != "" & data != "NULL") |> 
  select(-component_id) |> 
  # deploying all variables
  pivot_wider(
    names_from = data_id,
    values_from = data
  ) |>  
  rename(
    job = metier,
    sex = sexe,
    field = domaine
  ) |> 
  select(c(
    id:vviq, osviq, 
    contains("raven"), contains("sri"),
    vis_1:feel_3
  )
  ) |> 
  select(!contains("rt")) |> 
  unnest_wider(vviq) |>
  unnest_wider(osviq) |> 
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


# Isolating manual scoring data ------------------------------------------------

df_similarities <-
  df |> 
  # filtering out empty cells
  filter(data_id != "" & data != "NULL") |> 
  select(-component_id) |> 
  # deploying all variables
  pivot_wider(
    names_from = data_id,
    values_from = data
  ) |> 
  select(starts_with("simili"))

df_comprehension <-
  df |> 
  # filtering out empty cells
  filter(data_id != "" & data != "NULL") |> 
  select(-component_id) |> 
  # deploying all variables
  pivot_wider(
    names_from = data_id,
    values_from = data
  ) |> 
  select(starts_with("question"))


# All scores and classification ------------------------------------------------

df_final <-
  left_join(
    df_questionnaires,
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
    vviq = rowSums(across(starts_with("vviq_")), na.rm = TRUE),
    osivq_o = rowSums(across(starts_with("osivq_o_")), na.rm = TRUE),
    osivq_s = rowSums(across(starts_with("osivq_s_")), na.rm = TRUE),
    osivq_v = rowSums(across(starts_with("osivq_v_")), na.rm = TRUE),
    psiq_vis  = round((rowSums(across(starts_with("psiq_vis")),  na.rm = TRUE)/3),  digits = 2),
    psiq_aud  = round((rowSums(across(starts_with("psiq_aud")),  na.rm = TRUE)/3),  digits = 2),
    psiq_od   = round((rowSums(across(starts_with("psiq_od")),   na.rm = TRUE)/3),   digits = 2),
    psiq_gout = round((rowSums(across(starts_with("psiq_gout")), na.rm = TRUE)/3), digits = 2),
    psiq_tou  = round((rowSums(across(starts_with("psiq_tou")),  na.rm = TRUE)/3),  digits = 2),
    psiq_sens = round((rowSums(across(starts_with("psiq_sens")), na.rm = TRUE)/3), digits = 2),
    psiq_feel = round((rowSums(across(starts_with("psiq_feel")), na.rm = TRUE)/3), digits = 2),
    .keep = "unused"
  ) |> 
  mutate(
    # grouping by VVIQ according to convention
    group = ifelse(vviq <= 32, "Aphantasic", "Control"),
    group = as.factor(group)
  ) |>
  select(
    id, group, age:vision,
    vviq, osivq_o, osivq_s, osivq_v, 
    starts_with("psiq"), 
    score_raven, score_sri,
    span_spatial, span_digit,
    wcst_accuracy
  )


# Exporting back to Excel -------------------------------------------------

write.xlsx(
  list(
    "data"  = df_final,
    "similarities" = df_similarities,
    "comprehension" = df_comprehension
  ),
  "data/data-transformed/data_tidied.xlsx",
  asTable = TRUE,
  colNames = TRUE,
  colWidths = "auto",
  borders = "all",
  tableStyle = "TableStyleMedium16"
)

