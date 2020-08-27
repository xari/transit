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
mod_details_wrapper_ui <- function(id) {
  ns <- NS(id)

  div(class = "container trip_view_wrapper",
      div(
        id = "helper-text",
        class = "text-center mx-auto w-50 mb-4",
        p(
          "Once you've set your trip details using the controls above, you'll be able to view the proposed itineraries here below."
        )
      ),
      fluidRow(
        column(5,
               mod_connections_wrapper_ui(ns(
                 "connections_wrapper"
               ))),
        column(7,
               tabsetPanel(
                 tabPanel("Itinerary", gt::gt_output(ns("trip_gt"))),
                 tabPanel("Map", plotOutput(ns("map")))
               ))
      ))
}

# Module Serveri

#' @rdname mod_details_wrapper
#' @export
#' @keywords internal

mod_details_wrapper_server <- function(id, trip_details){
	moduleServer(id, function(input, output, session) {
			     ns <- session$ns

			     # Returns the connections table
			     # along with the index of the selected connection
			     connections <- mod_connections_wrapper_server("connections_wrapper",
									   trip_details)

			     observe({
				     req(connections$connections())

				     removeUI("#helper-text p")

				     insertUI("#helper-text",
					      "afterBegin",
					      p(class = "lead",
						"Bon voyage!"))
			     })

			     output$trip_gt <- gt::render_gt({
				     validate(need(!is.na(connections$selected_connection()),
						   "Please select a connections."))

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
				     validate(need(connections$selected_connection(),
						   "Please select one of the connections."))

				     hoist_stops_from_connections(connections$connections(),
								  connections$selected_connection()) %>%
				     get_map()
			     })
      })
}
