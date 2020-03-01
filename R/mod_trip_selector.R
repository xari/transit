# Module UI

#' @title   mod_trip_selector_ui and mod_trip_selector_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_trip_selector
#'
#' @keywords internal
#' @export
#' @importFrom shiny NS tagList
#' @import timepickerInput
mod_trip_selector_ui <- function(id){
  ns <- NS(id)

  div(
    class = "card card-body",
    div(
      class = "row row-cols-2",
      # From
      mod_station_selector_ui(ns("station_selector_ui_origin"),
                              label = "Origin"),
      # To
      mod_station_selector_ui(ns("station_selector_ui_destination"),
                              label = "Destination")
    ),
    div(
      id = "date_and_time",
      class = "collapse row row-cols-2",
      dateInput(ns("date"),
                "Date"),
      div(
        class = "form-group",
        tags$label(class = "control-label",
                   "Time"),
        timepickerInput(ns("time"),
                        configuration = list(disableClock = TRUE,
                                             format = "HH:mm",
                                             hourPlaceholder = "HH",
                                             minutePlaceholder = "MM"))
      )
    )
  )
}

# Module Server

#' @rdname mod_trip_selector
#' @export
#' @keywords internal

mod_trip_selector_server <- function(input, output, session){
  ns <- session$ns

  from <- # Store the selectize value
    callModule(mod_station_selector_server,
               "station_selector_ui_origin")

  to <- # Re-use the station selector
    callModule(mod_station_selector_server,
               "station_selector_ui_destination")

  trip_details <- reactiveValues(
    "from" = NULL,
    "to" = NULL,
    "date" = NULL
  )

  observeEvent(from$station,
               trip_details$from <- from$station)

  observeEvent(to$station,
               trip_details$to <- to$station)

  observeEvent(input$date,
               trip_details$date <- input$date)

  observeEvent(input$time,
               trip_details$time <- input$time)

  trip_details
}
