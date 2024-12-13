# read_xlsx("data/data-raw/metadata.xlsx") |> 
#   mutate(
#     Duration = Duration |> 
#       format(
#         format = "%H:%M:%S",
#         digits = 0
#       ) |> 
#       lubridate::as.difftime()
#   ) |> 
#   reframe(
#     Median = median(Duration),
#     MAD = mad(Duration))