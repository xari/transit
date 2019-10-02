# Module UI

#' @title   mod_connection_table_ui and mod_connection_table_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_connection_table
#'
#' @keywords internal
#' @export
#' @importFrom shiny NS tagList
mod_connection_table_ui <- function(id) {
  ns <- NS(id)

  DT::DTOutput(ns("connection_details"))
}

# Module Server

#' @rdname mod_connection_table
#' @export
#' @keywords internal

mod_connection_table_server <- function(input, output, session, data) {
  ns <- session$ns

  output$connection_details <- DT::renderDT(
    data,
    rownames = FALSE,
    options = list(
      info = FALSE,
      paging = FALSE,
      searching = FALSE
    )
  )
}
