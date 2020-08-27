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
  function(id) {
    moduleServer(id, function(input,
                              output,
                              session,
                              origin,
                              departure,
                              destination,
                              arrival,
                              sections) {
      ns <- session$ns

      output$card <-
        renderUI({
          div(class = "card",
              actionButton(
                inputId = ns("is_selected"),
                label = h4(
                  class = "card-title",
                  icon("stopwatch"),
                  paste(departure, "—", arrival)
                ),
                class = "card-body"
              ))
        })

      input
    })
  }
