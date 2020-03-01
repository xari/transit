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
#' @import shinieR
mod_trip_selector_ui <- function(id){
  ns <- NS(id)

  tagList(fluidRow(
    column(4,
           mod_station_selector_ui(
             ns("station_selector_ui_origin"),
             label = "Origin"
           )),
    column(4,
           mod_station_selector_ui(
             ns("station_selector_ui_destination"),
             label = "Destination"
           )),
    column(4,
           mod_station_selector_ui(
             ns("station_selector_ui_via"),
             label = "Via (optional)"
           ))
  ),

  fluidRow(column(4,
                  dateInput(ns(
                    "date"
                  ),
                  "Date")),
           column(
             4,
             div(
               class = "form-group",
               width = "25%",
               tags$label(class = "control-label",
                          "Time"),
               span(
                 style = "display: flex",
                 timeInput(
                   ns("time"),
                   configuration = list(
                     disableClock = TRUE,
                     format = "HH:mm",
                     hourPlaceholder = "HH",
                     minutePlaceholder = "MM"
                   )
                 ),
                 "isArrivalTime:",
                 switchInput(ns("isArrivalTime"))
               )
             )
           ),
           column(4)))
}

# Module Server

#' @rdname mod_trip_selector
#' @export
#' @keywords internal

mod_trip_selector_server <- function(input, output, session){
  ns <- session$ns

  from <-
    callModule(mod_station_selector_server,
               "station_selector_ui_origin")

  to <-
    callModule(mod_station_selector_server,
               "station_selector_ui_destination")

  via <-
    callModule(mod_station_selector_server,
               "station_selector_ui_via")

  trip_details <- reactiveValues(
    "from" = NULL,
    "to" = NULL,
    "via" = NULL,
    "date" = NULL,
    "time" = NULL,
    "isArrivalTime" = NULL
  )

  observeEvent(from$station,
               trip_details$from <- from$station)

  observeEvent(to$station,
               trip_details$to <- to$station)

  observeEvent(via$station,
               trip_details$via <- via$station)

  observeEvent(input$date,
               trip_details$date <- input$date)

  observeEvent(input$time,
               trip_details$time <- input$time)

  observeEvent(input$isArrivalTime,
               trip_details$isArrivalTime <- as.numeric(input$isArrivalTime))

  trip_details
}
