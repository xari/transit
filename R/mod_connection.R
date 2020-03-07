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

mod_connection_server <-
  function(input,
           output,
           session,
           origin,
           departure,
           destination,
           arrival,
           sections,
           transfers,
           duration) {
    ns <- session$ns

    # Create a GT to display the sections
    sections_gt <-
      sections %>%
      dplyr::select(-stops) %>%
      gt::gt() %>%
      gt::fmt_time(columns = gt::vars(departure, arrival),
                   time_style = 2)

    output$card <-
      renderUI({
        div(
          class = "card",
          div(
            class = "card-body",
            h4(
              class = "card-title",
              paste(departure,
                    "—",
                    arrival),
              tags$br(),
              icon("stopwatch"),
              duration
            ),
            sections_gt
          )
        )
      })
  }
