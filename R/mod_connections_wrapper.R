# Module UI

#' @title   mod_connections_wrapper_ui and mod_connections_wrapper_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_connections_wrapper
#'
#' @keywords internal
#' @export
#' @importFrom shiny NS tagList

mod_connections_wrapper_ui <- function(id) {
  ns <- NS(id)

  uiOutput(ns(
    "connections_wrapper"
  ))
}

# Module Server

#' @rdname mod_connections_wrapper
#' @export
#' @keywords internal

mod_connections_wrapper_server <-
  function(input,
           output,
           session,
           trip_details) {
    ns <- session$ns

    # Get the next 4 connections between
    # the origin and destination stations.
    # Returns: a tibble
    connections <-
      reactive({
        validate(need(trip_details()$from, 'Needs an origin.'),
                 need(trip_details()$to, 'Needs a destination.'))

        print(trip_details())

        get_connections(trip_details())
      })

    # Create a unique ID for each row of
    # the "connections" table
    # Returns: c("connection_number_1",
    #            "connection_number_2",
    #            ...)
    unique_module_ids <-
      reactive({
        purrr::map_chr(1:nrow(connections()),
                       ~ paste0("connection_number_", .))
      })

    output$connections_wrapper <-
      renderUI({
        # Call the module UI once for each
        # of the unique IDs created earlier.
        purrr::map(ns(unique_module_ids()),
                   mod_connection_ui)
      })

    selected_connection <- reactiveVal(NULL)

    observe({
      # Call the module server once for each
      # of the unique IDs created earlier.
      purrr::pmap(connections(), function(rowid, ...) {
        # Call the module using one of
        # the unique IDs created earlier
        is_selected <- callModule(
          mod_connection_server,
          unique_module_ids()[rowid],
          ... # Pass down the rest of the columns
        )

        # Attach a click event listener
        # to each actionButton
        observeEvent(is_selected$is_selected, {
          # Update to selected connection
          selected_connection(rowid)
        })
      })
    })

    # Return the connections table
    # and the index of the selected connection
    list(
      connections = connections,
      selected_connection = selected_connection
    )
  }
