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

get_next_connections <- function(origin, destination) {
  httr::GET(
    stringr::str_interp(
      "http://transport.opendata.ch/v1/connections?from=${origin}&to=${destination}"
    )
  ) %>%
    httr::content(as = "parsed")
}

get_tibble_from_connections <- function(data) {
  data %>%
    purrr::pluck("connections") %>%
    {
      tidyr::tibble(
        origin = purrr::map_chr(., ~ .x$from$location$name),
        departure = purrr::map_chr(
          .,
          ~ lubridate::ymd_hms(.x$from$departure, tz = "Europe/Berlin") %>%  format("%H:%M")
        ),
        destination = purrr::map_chr(., ~ .x$to$location$name),
        arrival = purrr::map_chr(
          .,
          ~ lubridate::ymd_hms(.x$to$arrival, tz = "Europe/Berlin") %>% format("%H:%M")
        ),
        sections = purrr::map(., "sections"),
        transfers = purrr::map_dbl(., "transfers"),
        duration = purrr::map_chr(., "duration")
      )
    }
}

get_connections_tibble <- function(origin, destination) {
  get_next_connections(origin, destination) %>% # Returns a list of connections
    get_tibble_from_connections() %>% # Creates tibble from JSON response
    tibble::rowid_to_column() # Creates rowid column
}

getConnectionTablesFromSections <- function(sections) {
  sections_tables <- sections %>% purrr::map_dfr(
    ~ tidyr::tibble(
      origin = .$departure$station$name,
      origin_x = .$arrival$station$coordinate$x,
      origin_y = .$arrival$station$coordinate$y,
      departure = lubridate::ymd_hms(.$departure$departure, tz = "Europe/Berlin") %>% format("%H:%M"),
      destination = .$arrival$station$name,
      destination_x = .$arrival$station$coordinate$x,
      destination_y = .$arrival$station$coordinate$y,
      arrival = lubridate::ymd_hms(.$arrival$arrival, tz = "Europe/Berlin") %>% format("%H:%M")
    )
  )
}
