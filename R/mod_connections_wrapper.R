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

  uiOutput(ns("connections_wrapper"))
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
    # the origin and destination stations
    connections <-
      reactive({
        get_connections_tibble(reactiveValuesToList(trip_details))
      })

    # Create a unique ID for each row of
    # the "connections" table
    unique_module_ids <-
      reactive({
        purrr::map_chr(1:nrow(connections()), ~ paste0("connection_number_", .))
      })

    output$connections_wrapper <-
      renderUI({
        # Call the module UI once for each
        # of the unique IDs created earlier
        purrr::map(ns(unique_module_ids()),
                   mod_connection_ui)
      })

    observe({
      # Call a module for each row of
      # the connections table
      purrr::pmap(connections(), function(rowid,
                                          sections,
                                          ...) {
        # Call the module using one of
        # the unique IDs created earlier
        callModule(
          mod_connection_server,
          unique_module_ids()[rowid],
          list(
            # Create a "sections" tibble from
            # the sections argument
            sections = getConnectionTablesFromSections(sections),
            # Spread the rest of the function
            # arguments into the list
            ...
          )
        )
      })
    })
  }
