# Module UI

#' @title   mod_connection_ui and mod_connection_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_connection
#'
#' @keywords internal
#' @export
#' @importFrom shiny NS tagList
mod_connection_ui <- function(id) {
  ns <- NS(id)

  uiOutput(ns("card"))
}

# Module Server

#' @rdname mod_connection
#' @export
#' @keywords internal
#' @import gt
mod_connection_server <-
  function(input,
           output,
           session,
           origin,
           departure,
           destination,
           arrival,
           sections) {
    ns <- session$ns

    # Create a GT to display the
    # sections of each connection.
    sections_gt <-
      sections %>%
      dplyr::select(-stops) %>%
      gt() %>%
      fmt_time(columns = vars(departure, arrival),
               time_style = 2)

    output$card <-
      renderUI({
        div(class = "card",
            div(class = "card-body",
                h4(
                  class = "card-title",
                  icon("stopwatch"),
                  paste(departure, "—", arrival)
                ),
                sections_gt))
      })
  }
