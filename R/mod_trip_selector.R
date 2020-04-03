top_row <- function(ns) {
  fluidRow(
    column(
      4,
      mod_station_selector_ui(
        ns("station_selector_ui_origin"),
        label = "Origin*",
        placeholder = "Nyon"
      )
    ),
    column(
      4,
      mod_station_selector_ui(
        ns("station_selector_ui_destination"),
        label = "Destination*",
        placeholder = "Basel SBB"
      )
    ),
    column(
      4,
      mod_station_selector_ui(ns("station_selector_ui_via"),
                              label = "Via (optional)",
                              placeholder = "Bern")
    )
  )
}

time_inputs <- function(ns) {
  div(
    class = "form-group shiny-input-container switch-container",
    "Time",
    timeInput(
      ns("time"),
      configuration = list(
        disableClock = TRUE,
        format = "HH:mm",
        hourPlaceholder = "HH",
        minutePlaceholder = "MM"
      )
    ),
    switchInput(ns("isArrivalTime"),
                c("Departing", "Arriving"),
                configuration = list(icons = list(
                  checked = NULL,
                  unchecked = NULL
                )))
  )
}

bottom_row <- function(ns) {
  fluidRow(column(4,
                  dateInput(ns("date"),
                            "Date")),
           column(4,
                  time_inputs(ns)),
           column(4,
                  div(
                    class = "form-group shiny-input-container",
                    tags$label(class = "control-label",
                               "Ready?",
                               `for` = ns("submit_btn")),
                    actionButton(
                      ns("submit_btn"),
                      label = "Find connection",
                      icon = icon("search"),
                      class = "btn-primary",
                      style = "display: block"
                    ))))
}

# Module UI

#' @title   mod_trip_selector_ui and mod_trip_selector_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_trip_selector
#'
#' @keywords internal
#' @export
#' @importFrom shiny NS tagList
#' @import shinieR
mod_trip_selector_ui <- function(id) {
  ns <- NS(id)

  tagList(
    top_row(ns),
    bottom_row(ns)
  )
}

# Module Server

#' @rdname mod_trip_selector
#' @export
#' @keywords internal

mod_trip_selector_server <- function(input, output, session){
  ns <- session$ns

  from <-
    callModule(mod_station_selector_server,
               "station_selector_ui_origin")

  to <-
    callModule(mod_station_selector_server,
               "station_selector_ui_destination")

  via <-
    callModule(mod_station_selector_server,
               "station_selector_ui_via")

  observeEvent(input$submit_btn, {
    req(from$station, 'Needs an origin.',
        to$station, 'Needs a destination.')

    updateActionButton(session,
                       "submit_btn",
                       label = "Update",
                       icon = icon("redo"))
  })

  eventReactive(
    input$submit_btn,
    list(
      "from" = isolate(from$station),
      "to" = isolate(to$station),
      "via" = isolate(via$station),
      "date" = isolate(input$date),
      "time" = isolate(input$time),
      "isArrivalTime" = isolate(as.numeric(input$isArrivalTime))
    )
  )
}
