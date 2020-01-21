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

  div(uiOutput(ns("overview")),
      shinydashboardPlus::accordion(uiOutput(ns("details"))))
}

# Module Server

#' @rdname mod_connection
#' @export
#' @keywords internal

mod_connection_server <-
  function(input, output, session, data) {
    ns <- session$ns

    output$overview <-
      renderUI(
        div(
          div(
            span(data$origin),
            icon("map-marker-alt"),
            span(data$destination)
          ),
          div(
            span(data$departure),
            icon("stopwatch"),
            span(data$arrival)
          ),
          # Show one train icon per section
          div(purrr::map(0:data$transfers, ~ icon("train"))),
          div(data$duration)
        )
      )

    output$details <- renderUI(
      shinydashboardPlus::accordionItem(
        id = ns("details"),
        title = "Show itinerary",
        color = "danger",
        collapsed = TRUE,
        div(class = "card",
            div(
              class = "collapse show",
              div(
                class = "card-body",
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
            ))
      )
    )
  }
