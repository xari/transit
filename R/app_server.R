#' @import shiny
app_server <- function(input, output,session) {
  origin <-
    callModule(mod_station_selector_server,
               "station_selector_ui_origin")

  destination <-
    callModule(mod_station_selector_server,
               "station_selector_ui_destination")

  callModule(mod_connections_server,
             "connections_ui",
             origin,
             destination)
}
