getStations <- "function(query, callback) {
  if (!query.length) return callback();

  fetch('http://transport.opendata.ch/v1/locations?query='+encodeURIComponent(query)+'&limit=10')
    .then(function(response) {
      return response.json();
  })
    .then(function(responseJSON) {
      callback(responseJSON.stations.map(station => ({
        id: station.id,
        name: station.name
      })));
  });
}"

get_locations <- function(search_string) {
  httr::GET(
    stringr::str_interp(
      "http://transport.opendata.ch/v1/locations?query=${search_string}"
    )
  ) %>%
    httr::content(as = "parsed") %>%
    purrr::pluck("stations")
}

get_next_departures <- function(origin, destination) {
  httr::GET(
    stringr::str_interp(
      "http://transport.opendata.ch/v1/connections?from=${origin}&to=${destination}"
    )
  ) %>%
    httr::content(as = "parsed") %>%
    purrr::pluck("connections") %>%
    {
      tidyr::tibble(
        origin = purrr::map_chr(., ~ .x$from$location$name),
        departure = purrr::map_chr(., ~ .x$from$departure),
        destination = purrr::map_chr(., ~ .x$to$location$name),
        arrival = purrr::map_chr(., ~ .x$to$arrival),
        sections = purrr::map(., "sections"),
        transfers = purrr::map_dbl(., "transfers"),
        duration = purrr::map_chr(., "duration")
      )
    } %>%
    dplyr::mutate(
      departure = lubridate::ymd_hms(departure, tz = "Europe/Berlin"),
      arrival = lubridate::ymd_hms(arrival, tz = "Europe/Berlin"),
      sections = sections %>% purrr::map_depth(
        2,
        ~ list(
          # "name" = .$journey$name,
          "origin" = .$departure$station$name,
          "origin_x" = .$arrival$station$coordinate$x,
          "origin_y" = .$arrival$station$coordinate$y,
          "departure" = lubridate::ymd_hms(.$departure$departure, tz = "Europe/Berlin"),
          "destination" = .$arrival$station$name,
          "destination_x" = .$arrival$station$coordinate$x,
          "destination_y" = .$arrival$station$coordinate$y,
          "arrival" = lubridate::ymd_hms(.$arrival$arrival, tz = "Europe/Berlin")
        )
      ) %>%
        purrr::map( ~ purrr::map_dfr(., tidyr::as_tibble))
    )
}

getConnectionTable <- function(sections) {
  sections %>%
    dplyr::select(-origin_x, -origin_y, -destination_x, -destination_y)
}
