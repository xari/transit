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

create_url_from_args <- function(from, # required 	Specifies the departure location of the connection 	Lausanne
                                 to, # required 	Specifies the arrival location of the connection 	Genève
                                 via = NULL, # optional 	Specifies up to five via locations. When specifying several vias, array notation (via[]=via1&via[]=via2) is required. 	Bern
                                 date = NULL, # optional 	Date of the connection, in the format YYYY-MM-DD 	2012-03-25
                                 time = NULL, # optional 	Time of the connection, in the format hh:mm 	17:30
                                 isArrivalTime = NULL, # optional 	defaults to 0, if set to 1 the passed date and time is the arrival time 	1
                                 transportations = NULL, # optional 	Transportation means; one or more of train, tram, ship, bus, cableway 	transportations[]=tram&transportations[]=bus
                                 limit = NULL, # optional 	1 - 16. Specifies the number of connections to return. If several connections depart at the same time they are counted as 1. 	4
                                 page = NULL, # optional 	0 - 3. Allows pagination of connections. Zero-based, so first page is 0, second is 1, third is 2 and so on. 	3
                                 direct = NULL, # optional 	defaults to 0, if set to 1 only direct connections are allowed 	1
                                 sleeper = NULL, # optional 	defaults to 0, if set to 1 only night trains containing beds are allowed, implies direct=1 	1
                                 couchette = NULL, # optional 	defaults to 0, if set to 1 only night trains containing couchettes are allowed, implies direct=1 	1
                                 bike = NULL, # optional 	defaults to 0, if set to 1 only trains allowing the transport of bicycles are allowed 	1
                                 accessibility = NULL # optional 	Possible values are independent_boarding, assisted_boarding, and advanced_notice 	independent_boarding
                              ) {
  params <- as.list(match.call()) %>% tail(n = -1)

  url <- ""
  urltools::scheme(url) <- "http"
  urltools::domain(url) <- "transport.opendata.ch"
  urltools::path(url) <- "v1/connections"

  for (n in names(params)) {
    if(!is.null(params[[n]])) {
      url <- urltools::param_set(url, n, params[[n]])
    }
  }

  url
}

make_api_request <- function(url) {
  httr::GET(url) %>%
    httr::content(as = "parsed")
}

create_tibble_from_api_response <- function(data) {
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
        duration = purrr::map(., ~ {
          hms <- .x$duration %>%
            stringr::str_remove("00d") %>% # Remove days
            stringr::str_remove(":00") %>% # Remove seconds
            stringr::str_split(":")

          lubridate::duration(units = "hours",
                              hour = as.numeric(hms[[1]][1]),
                              minute = as.numeric(hms[[1]][2]))
        })
      ) %>%
        tibble::rowid_to_column() # Creates rowid column
    }
}

create_tibble_from_stops <- function(stops) {
  purrr::map_dfr(
    stops,
    ~ tidyr::tibble(
      station = .$location$name,
      x = .$location$coordinate$x,
      y = .$location$coordinate$y,
      type = .$location$coordinate$type
    )
  )
}

create_tibble_from_sections <- function(sections) {
  purrr::map(sections,
             ~ purrr::map_dfr(
               .,
               ~
                 tidyr::tibble(
                   origin = .$departure$station$name,
                   departure = lubridate::ymd_hms(.$departure$departure, tz = "Europe/Berlin") %>% format("%H:%M"),
                   destination = .$arrival$station$name,
                   arrival = lubridate::ymd_hms(.$arrival$arrival, tz = "Europe/Berlin") %>% format("%H:%M"),
                   stops = list(stops = create_tibble_from_stops(.$journey$passList))
                 )
             ))
}

get_connections <- function(trip_details) {
  # Spreads list items into arguments
  do.call(create_url_from_args, trip_details) %>% # Returns a list of connections
    make_api_request() %>%
    create_tibble_from_api_response() %>%
    dplyr::mutate(sections = create_tibble_from_sections(sections))
}
