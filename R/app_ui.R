#' @import shiny
app_ui <- function() {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here
    fluidPage(
      theme = shinythemes::shinytheme("cosmo"),
      titlePanel("transit"),

      # Station selector...
      div(
        # From
        mod_station_selector_ui("station_selector_ui_origin",
                                label = "Select your origin."),
        # To
        mod_station_selector_ui("station_selector_ui_destination",
                                label = "Select your destination."),
        # Day...
        dateInput(
          "date",
          "Date"
        ),
        # ShinyTime
        tags$input(id = "timepicker",
                   type = "text",
                   class = "timepicker"),
      ),

      # Time table
      mod_connections_wrapper_ui("connections_wrapper"),

      tags$script(src = "www/main.js")
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
    golem::favicon(),
    # Add here all the external resources
    # If you have a custom.css in the inst/app/www
    # Or for example, you can add shinyalert::useShinyalert() here
    #tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
    tags$link(rel = "stylesheet", type = "text/css", href = "www/jquery.timepicker.min.css"),
    tags$script(src = "www/jquery.timepicker.min.js")
  )
}
