#' @import shiny
app_server <- function(input, output,session) {
  trip_details <- callModule(mod_trip_selector_server,
                             "trip_selector")

  callModule(mod_connections_wrapper_server,
             "connections_wrapper",
             trip_details)
}
