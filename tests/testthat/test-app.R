library(leaflet)

test_that("Connections are returned", {
  trip_details = list(from = "8500090",
                      to = "8501008")

  url <- do.call(create_url_from_args, trip_details)

  raw_data <- make_api_request(url)

  connections_tibble <- create_tibble_from_api_response(raw_data)

  connections_tibble <-
    connections_tibble %>%
    dplyr::mutate(sections = create_tibble_from_sections(sections))

  readr::write_rds(connections_tibble,
                   path = "./example_connection_gva_bsl.rds")

  # For GT
  connections_tibble %>%
    dplyr::slice(1) %>%
    purrr::pluck("sections") %>%
    dplyr::first() %>%
    dplyr::select(-stops) %>%
    dplyr::mutate(
      walk = ifelse(!is.na(walk),
                    paste(walk / 60, "minute walk"),
                    NA),
      product = paste(category, number),
      description = ifelse(is.na(walk),
                           product,
                           walk)
    ) %>%
    dplyr::select(-category,-number,-walk, -product)

  hoist_stops_from_connections(connections_tibble, 3) %>%
    get_map()
})
