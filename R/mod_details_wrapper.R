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

  div(class="shadow-sm",
    gt::gt_output(ns("trip_gt"))
  )
}

# Module Server

#' @rdname mod_details_wrapper
#' @export
#' @keywords internal

mod_details_wrapper_server <- function(id, trip_details){
	moduleServer(id, function(input, output, session) {
			     ns <- session$ns

			      # Returns: a tibble
			      connection <-
				reactive({
				  validate(
				    need(trip_details()$from, 'Needs an origin.'),
				    need(trip_details()$to, 'Needs a destination.')
				  )

				  get_connections(trip_details()) %>%
				    dplyr::slice(1)
				})

			     # Make sure that a connection has been selected from the buttons.
			     # Then use that selection to pass the
			     output$trip_gt <- gt::render_gt({
				     validate(need(!is.na(connection()),
						   "Please select a connection."))

				     title <- paste(
						    connection() %>%
							    purrr::pluck("origin"),
						    "—",
						    connection() %>%
							    purrr::pluck("destination")
				     )

				     subtitle <- paste(
						       connection() %>%
							       purrr::pluck("departure"),
						       "—",
						       connection() %>%
							       purrr::pluck("arrival")
				     )

				     format_data_for_gt(connection()) %>%
					     get_gt(title, subtitle)
			     })
      })
}
