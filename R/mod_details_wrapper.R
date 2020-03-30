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
    gt::gt_output(ns("trip_gt")),
    plotOutput(ns("map"))
  )
}

# Module Server

#' @rdname mod_details_wrapper
#' @export
#' @keywords internal

mod_details_wrapper_server <- function(input, output, session, connections){
  ns <- session$ns

  selected_connection <- reactive({
    validate(need(connections$selected_connection(),
                  "Please select a connection from the list."))

    connections$connections() %>%
      dplyr::slice(connections$selected_connection())
  })

  title <- reactive(paste(
    selected_connection() %>%
      purrr::pluck("origin"),
    "—",
    selected_connection() %>%
      purrr::pluck("destination")
  ))

  subtitle <- reactive(paste(
    selected_connection() %>%
      purrr::pluck("departure"),
    "—",
    selected_connection() %>%
      purrr::pluck("arrival")
  ))

  output$trip_gt <- gt::render_gt({
    # Create a GT to display the
    # sections of each connection.
    selected_connection() %>%
      purrr::pluck("sections") %>%
      dplyr::first() %>%
      dplyr::select(-stops) %>%
      dplyr::mutate(
        walk = ifelse(!is.na(walk),
                      paste(walk / 60, "minute walk"),
                      NA),
        product = paste(category, number),
        description = ifelse(is.na(walk),
                             product,
                             walk)
      ) %>%
      dplyr::select(-category,-number,-walk, -product) %>%
      gt() %>%
      tab_header(
        title = title(),
        subtitle = subtitle()
      ) %>%
      tab_spanner(label = "Departing",
                  columns = vars(origin, departure)) %>%
      tab_spanner(label = "Arriving",
                  columns = vars(destination, arrival)) %>%
      cols_move_to_end(columns = vars(origin, departure, arrival, destination)) %>%
      cols_label(
        origin = "",
        departure = "",
        destination = "",
        arrival = "",
        description = ""
      ) %>%
      fmt_time(columns = vars(departure, arrival),
               time_style = 2) %>%
      opt_row_striping()
  })


  # stops <- reactive({
  #   connections$connections() %>%
  #     tidyr::hoist(sections, stops = "stops") %>%
  #     dplyr::select(rowid, stops) %>%
  #     tidyr::unnest_longer("stops") %>%
  #     tidyr::unnest(cols = c(stops)) %>%
  #     dplyr::select(-stops) %>%
  #     tidyr::drop_na() %>%
  #     tidyr::nest(stops = c("station", "x", "y")) %>%
  #     dplyr::slice(connections$selected_connection()) %>%
  #     tidyr::unnest(stops)
  # })
  #
  # output$map <- renderPlot({
  #   print(stops())
  #
  #   stops() %>%
  #     {
  #       ggmap::qmplot(
  #         geom = "line",
  #         x = y,
  #         y = x,
  #         data = .,
  #         maptype = "toner-lines",
  #         source = "osm",
  #         color = I("red")
  #       )
  #     }
  # })
}
