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

mod_station_selector_ui <-
  function(id, label = "Select a station.") {
    ns <- NS(id)

    selectizeInput(
      ns("station"),
      label,
      choices = NULL,
      options = list(
        valueField = 'id',
        labelField = 'name',
        searchField = 'name',
        load = I(getStations)
      ),
    )
  }

# Module Server

#' @rdname mod_station_selector
#' @export
#' @keywords internal

mod_station_selector_server <-
  function(input, output, session) {
    ns <- session$ns

    input
  }
