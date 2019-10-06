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

  tagList(
    textOutput(ns("overview")),
    shinydashboardPlus::accordion(uiOutput(ns("connection")))
  )
}

# Module Server

#' @rdname mod_connection_table
#' @export
#' @keywords internal

mod_connection_table_server <-
  function(input, output, session, data) {
    ns <- session$ns

    output$overview <- renderText(paste(data$departure, data$arrival, data$duration, data$transfers))

    output$connection <- renderUI(
      shinydashboardPlus::accordionItem(
        id = ns("connection"),
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
                    dplyr::select(-origin_x,-origin_y,-destination_x,-destination_y),
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

    # output$connection <- DT::renderDT(
    #   data$sections,
    #   rownames = FALSE,
    #   options = list(
    #     info = FALSE,
    #     paging = FALSE,
    #     searching = FALSE
    #   )
    # )
  }
