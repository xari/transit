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

  div(class = "card",
      div(
        class = "card-body",
        uiOutput(ns("overview")),
        div(
          class = "text-center",
          shinydashboardPlus::accordion(uiOutput(ns("details")))
        )
      ))
}

# Module Server

#' @rdname mod_connection
#' @export
#' @keywords internal

mod_connection_server <-
  function(input, output, session, data) {
    ns <- session$ns

    output$overview <-
      renderUI(tagList(
        h4(
          class = "text-center",
          data$origin,
          icon("stopwatch"),
          data$departure,
          " — ",
          data$destination,
          data$arrival
        ),
        h3(
          class = "text-center",
          data$duration,
          " ",
          purrr::map(0:data$transfers, ~ icon("train")),
        )
      ))

    output$details <- renderUI(
      shinydashboardPlus::accordionItem(
        id = ns("details"),
        title = "Show itinerary",
        color = "danger",
        collapsed = TRUE,
        div(
          class = "collapse show",
          renderTable(
            data$sections %>%
              dplyr::select(-origin_x, -origin_y, -destination_x, -destination_y),
            rownames = FALSE,
            options = list(
              info = FALSE,
              paging = FALSE,
              searching = FALSE
            )
          )
        )
      )
    )
  }
