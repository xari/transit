
#' @import shiny
app_ui <- function() {
  bootstraplib::bs_theme_new(version = "4+3",
                             bootswatch = "cosmo")

  bootstraplib::bs_theme_add_variables(
    primary = "#d30a09",
    "body-bg" = "#fff",
    "font-family-base" = "monospace"
  )

  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here
    fluidPage(
      bootstraplib::bootstrap(),
      titlePanel("transit"),

      # Station selector...
      div(
        class = "trip_planner_wrapper container",
        div(
          # From
          mod_station_selector_ui("station_selector_ui_origin",
                                  label = "Origin"),
          # To
          mod_station_selector_ui("station_selector_ui_destination",
                                  label = "Destination")
        ),
        div(
            id = "date_and_time",
            class = "d-none",
            # Day...
            dateInput("date",
                      "Date"),
            # ShinyTime
            div(
              class = "form-group shiny-input-container",
              tags$label(class = "control-label",
                         "Time"),
              tags$input(id = "timepicker",
                         type = "text",
                         class = "form-control timepicker")
            )
        )
      ),

      div(
        class = "container",
        img(src = "https://sbb.imgix.net/content/dam/internet/sharedimages/home/Swisspass-Jugendlicher-Blauband.jpg?crop=focalpoint&fp-x=0.49&fp-y=0.13166666&fp-z=1&w=1967&h=1273&auto=format,compress,cs=tinysrgb&q=30",
            alt = "Banner image")
      ),

      # Time table
      mod_connections_wrapper_ui("connections_wrapper"),

      tags$script(src = "www/main.js")
    )
  )
}

sass::sass(
  sass::sass_file("scss/selectize.scss"),
  output = "inst/app/www/main.css"
)

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
    tags$link(rel = "stylesheet", type = "text/css", href = "www/main.css"),
    tags$script(src = "www/jquery.timepicker.min.js")
  )
}
