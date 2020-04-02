
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
        sections = purrr::map(., "sections")
      ) %>%
        tibble::rowid_to_column() # Creates rowid column
    }
}

create_tibble_from_stops <- function(stops) {
  purrr::map_dfr(
    stops,
    ~
      tidyr::tibble(
        station = .$location$name,
        x = .$location$coordinate$x,
        y = .$location$coordinate$y
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
                   category = ifelse(!is.null(.$journey), .$journey$category, NA),
                   number = ifelse(!is.null(.$journey), .$journey$number, NA),
                   walk = ifelse(!is.null(.$walk), .$walk$duration, NA),
                   stops = ifelse(!is.null(.$journey),
                                  list(stops = create_tibble_from_stops(.$journey$passList)),
                                  list(stops = NA))
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

format_data_for_gt <- function(selected_connection) {
  selected_connection %>%
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
    dplyr::select(-category, -number, -walk,-product)
}

#' Create a GT to display the sections for each connection
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
get_gt <- function(selected_connection, title, subtitle) {
  selected_connection %>%
    gt() %>%
    tab_options(container.width = pct(100),
                table.width = pct(100)) %>%
    tab_header(title = title,
               subtitle = subtitle) %>%
    tab_spanner(label = "Departing",
                columns = vars(origin, departure)) %>%
    tab_spanner(label = "Arriving",
                columns = vars(destination, arrival)) %>%
    cols_move_to_end(columns = vars(origin, departure, arrival, destination)) %>%
    cols_label(
      origin = "",
      departure = "",
      destination = "",
      arrival = "",
      description = ""
    ) %>%
    fmt_time(columns = vars(departure, arrival),
             time_style = 2) %>%
    opt_row_striping()
}

hoist_stops_from_connections <- function(connections) {
  connections %>%
    tidyr::hoist(sections, stops = "stops") %>%
    dplyr::select(rowid, stops) %>%
    tidyr::unnest_longer("stops") %>%
    tidyr::unnest(cols = c(stops)) %>%
    # dplyr::select(-stops) %>%
    tidyr::drop_na()
}

get_map <- function(data, selected_connection) {
  selected_connection <-
    data %>%
    dplyr::filter(rowid == selected_connection)

  ggmap::qmplot(
    geom = "line",
    x = y,
    y = x,
    data = data,
    maptype = "toner-lite",
    # force = TRUE,
    color = I("#8c8c8c")
  ) +
    {
      ggplot2::geom_line(
        data = selected_connection,
        ggplot2::aes(x = y,
                     y = x),
        color = "#d30a09"
      )
    } +
    ggplot2::theme(legend.position = "none")
}
