#' @import shiny
app_server <- function(input, output,session) {
  origin <- # Store the selectize value
    callModule(mod_station_selector_server,
               "station_selector_ui_origin")

  destination <- # Re-use the station selector
    callModule(mod_station_selector_server,
               "station_selector_ui_destination")

  callModule(mod_connections_wrapper_server,
             "connections_wrapper",
             origin, # Pass-down the selectize values
             destination)
}
