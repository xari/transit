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

  tagList(textOutput(ns("text")),
          DT::DTOutput(ns("connection_details")))
}

# Module Server

#' @rdname mod_connection
#' @export
#' @keywords internal

mod_connection_server <- function(input, output, session, ...) {
  ns <- session$ns

  args <- list(...)

  output$text <- renderText(args$transfers)

  output$connection_details <- DT::renderDT(
    args$sections %>%
      dplyr::select(-origin_x,-origin_y,-destination_x,-destination_y),
    rownames = FALSE,
    options = list(
      info = FALSE,
      paging = FALSE,
      searching = FALSE
    )
  )
}
