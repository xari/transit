
# Remember that UI code doesn't have to be put directly inside 
# the module's root UI function.
#
# In this case, we're keeping our UI a little easier to read by
# breaking it up into three separate functions that we can call
# from inside of our module's root UI function.
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

# This piece contains to inputs that relate to the "time" portion
# of our journey.
# Both inputs are custom Shiny inputs that come from the ShinieR
# package that we'll explore in another subject when we learn how
# to package Shiny modules in an R package that we can use in
# multiple Shiny apps.
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

# Bottom row will render the time_inputs.
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

mod_trip_selector_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Render three station selector modules, and return
    # the input value from each.
    from <-
      mod_station_selector_server("station_selector_ui_origin")

    to <-
      mod_station_selector_server("station_selector_ui_destination")

    via <-
      mod_station_selector_server("station_selector_ui_via")

    # Wouldn't it be cool if the submit button updated it's text
    # after the first time that its rendered?
    # Well... It can do just that if we attach a call to updateActionButton()
    # to a click-event of the submit button.
    # A click-event just means "when a user clicks on the button".
    observeEvent(input$submit_btn, {
      req(from$station,
          'Needs an origin.',
          to$station,
          'Needs a destination.')

      updateActionButton(session,
                         "submit_btn",
                         label = "Update",
                         icon = icon("redo"))
    })

    # Return the form values whenever the submit button is clicked.
    eventReactive(
      input$submit_btn,
      list(
        "from" = from$station,
        "to" = to$station,
        "via" = via$station,
        "date" = input$date,
        "time" = input$time,
        "isArrivalTime" = as.numeric(input$isArrivalTime)
      )
    )
  })
}
