stations <-
  readxl::read_xlsx("data-raw/bavlistcurrenttimetable.xlsx") %>%
  dplyr::select("Dst-Nr85", "Name") %>%
  tail(-3) %>%
  dplyr::rename(value = "Dst-Nr85", label = "Name")

usethis::use_data(stations, overwrite = TRUE)
