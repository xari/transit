# Module UI

#' @title   mod_details_wrapper_ui and mod_details_wrapper_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_details_wrapper
#'
#' @keywords internal
#' @export
#' @importFrom shiny NS tagList
mod_details_wrapper_ui <- function(id){
  ns <- NS(id)

  uiOutput("details_wrapper")
}

# Module Server

#' @rdname mod_details_wrapper
#' @export
#' @keywords internal

mod_details_wrapper_server <- function(input, output, session, connections){
  ns <- session$ns

  observe({
    insertUI(
      selector = "#details_wrapper",
      where = "afterBegin",
      ui = tagList(gt::gt_output(ns("trip_gt")),
                   plotOutput(ns("map")))
    )

    req(!is.null(connections$selected_connection()))

    output$trip_gt <- gt::render_gt({
      unique_connection <-
        connections$connections() %>%
        dplyr::slice(connections$selected_connection())

      title <- paste(
        unique_connection %>%
          purrr::pluck("origin"),
        "—",
        unique_connection %>%
          purrr::pluck("destination")
      )

      subtitle <- paste(
        unique_connection %>%
          purrr::pluck("departure"),
        "—",
        unique_connection %>%
          purrr::pluck("arrival")
      )

      format_data_for_gt(unique_connection) %>%
        get_gt(title, subtitle)
    })

    output$map <- renderPlot({
      hoist_stops_from_connections(connections$connections()) %>%
        get_map(connections$selected_connection())
    })
  })
}
