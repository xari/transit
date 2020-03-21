#' @import shiny
app_server <- function(input, output,session) {
  trip_details <- callModule(mod_trip_selector_server,
                             "trip_selector")



  connections <- callModule(mod_connections_wrapper_server,
             "connections_wrapper",
             trip_details)

  callModule(mod_details_wrapper_server,
             "details_wrapper",
             connections)
}
