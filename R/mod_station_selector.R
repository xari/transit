# Module UI

#' @title   mod_station_selector_ui and mod_station_selector_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_station_selector
#'
#' @keywords internal
#' @export
#' @importFrom shiny NS tagList
mod_station_selector_ui <- function(id) {
  ns <- NS(id)
  tagList(
    useStationSelector(ns("origin"), "Select your departure"),
    uiOutput(ns("destination")),
    uiOutput(ns("connections"))
  )
}

# Module Server

#' @rdname mod_station_selector
#' @export
#' @keywords internal

mod_station_selector_server <- function(input, output, session) {
  ns <- session$ns

  output$destination <- renderUI({
    useStationSelector(ns("destination"), "Select your destination")
  })

  output$connections <-
    renderUI({
      get_next_departures(input$origin, input$destination) %>%
        tibble::rowid_to_column() %>%
        purrr::pmap(initConnectionModule)
    })
}
