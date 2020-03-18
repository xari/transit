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
  tagList(
    gt::gt_output(ns("trip_gt"))
  )
}

# Module Server

#' @rdname mod_details_wrapper
#' @export
#' @keywords internal

mod_details_wrapper_server <- function(input, output, session, connections){
  ns <- session$ns

  selected_connection <- reactive({
    connections$connections() %>%
      dplyr::slice(connections$selected_connection())
  })

  output$trip_gt <- gt::render_gt({
    # Create a GT to display the
    # sections of each connection.
    selected_connection()[1, ] %>%
      purrr::pluck(sections) %>%
      dplyr::as_tibble() %>%
      dplyr::select(-stops) %>%
      gt() %>%
      fmt_time(columns = vars(departure, arrival),
               time_style = 2)
  })
}
