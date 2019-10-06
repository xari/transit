#' @import shiny
app_ui <- function() {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here
    fluidPage(
      theme = shinythemes::shinytheme("cosmo"),
      titlePanel("transit"),
      sidebarLayout(
        sidebarPanel(
          mod_station_selector_ui("station_selector_ui_origin",
                                  label = "Select your origin."),
          # Re-use the station selector
          mod_station_selector_ui("station_selector_ui_destination",
                                  label = "Select your destination.")
        ),
        mainPanel(mod_connections_wrapper_ui("connections_wrapper"))
      )
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
