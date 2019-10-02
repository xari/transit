#' @import shiny
app_ui <- function() {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here
    fluidPage(
      h1("transit"),
      origin_ui <-
        mod_station_selector_ui("station_selector_ui_origin",
                                label = "Select your origin."),
      destination_ui <-
        mod_station_selector_ui("station_selector_ui_destination",
                                label = "Select your destination."),
      mod_connections_ui("connections_ui")
    )
  )
}

#' @import shiny
golem_add_external_resources <- function(){

  addResourcePath(
    'www', system.file('app/www', package = 'transit')
  )

  tags$head(
    golem::activate_js(),
    golem::favicon()
    # Add here all the external resources
    # If you have a custom.css in the inst/app/www
    # Or for example, you can add shinyalert::useShinyalert() here
    #tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
  )
}
