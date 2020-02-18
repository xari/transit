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
      dateInput("date",
                "Date"),
      div(
        class = "form-group shiny-input-container",
        tags$label(class = "control-label",
                   "Time"),
        tags$input(id = "timepicker",
                   type = "text",
                   class = "form-control timepicker")
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

  origin <- # Store the selectize value
    callModule(mod_station_selector_server,
               "station_selector_ui_origin")

  destination <- # Re-use the station selector
    callModule(mod_station_selector_server,
               "station_selector_ui_destination")

  list("origin" = origin,
       "destination" = destination)
}
