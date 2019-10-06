#' @import shiny
app_server <- function(input, output,session) {
  origin <- # Store the selectize value
    callModule(mod_station_selector_server,
               "station_selector_ui_origin")

  destination <- # Re-use the station selector
    callModule(mod_station_selector_server,
               "station_selector_ui_destination")

  callModule(mod_connections_server,
             "connections_ui",
             origin, # Pass-down the selectize values
             destination)
}
