test_that("Connections are returned", {
  trip_details = list(from = "Geneva",
                      to = "Basel")

  url <- do.call(create_url_from_args, trip_details)

  raw_data <- make_api_request(url)

  connections_tibble <- create_tibble_from_api_response(raw_data)

  connections_tibble <-
    connections_tibble %>%
    dplyr::mutate(sections = create_tibble_from_sections(sections))


})
