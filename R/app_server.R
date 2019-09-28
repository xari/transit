#' @import shiny
app_server <- function(input, output,session) {
  callModule(mod_station_selector_server, "station_selector_ui_1")
}
