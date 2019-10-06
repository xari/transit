# Module UI

#' @title   mod_connections_ui and mod_connections_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_connections
#'
#' @keywords internal
#' @export
#' @importFrom shiny NS tagList

mod_connections_ui <- function(id) {
  ns <- NS(id)

  uiOutput(ns("connections_wrapper"))
}

# Module Server

#' @rdname mod_connections
#' @export
#' @keywords internal

mod_connections_server <-
  function(input,
           output,
           session,
           origin,
           destination) {
    ns <- session$ns

    # Get next 4 connections between origin and destination
    connections <- reactive(get_connections_tibble(origin$station, destination$station))

    # Create module IDs
    unique_module_ids <-
      reactive(purrr::map_chr(1:nrow(connections()), ~ paste0("connection_number_", .)))

    output$connections_wrapper <-
      renderUI({
        purrr::map(ns(unique_module_ids()),
                   mod_connection_table_ui)
      })

    observe({
      purrr::pmap(connections(), function(rowid,
                                          departure,
                                          arrival,
                                          duration,
                                          transfers,
                                          sections,
                                          ...) {
        # Call modules using unique ID
        callModule(
          mod_connection_table_server,
          unique_module_ids()[rowid],
          # The unique ID
          list(
            # Pass connections data to module
            departure = departure,
            arrival = arrival,
            duration = duration,
            transfers = transfers,
            sections = getConnectionTablesFromSections(sections)
          )
        )
      })
    })

    # reactive(connections() %>%
    #            purrr::pmap(function(...) {
    #              args <- list(...) # The list of connection details
    #
    #              # Call modules using unique ID
    #              callModule(
    #                mod_connection_table_server,
    #                module_unique_id,
    #                list(
    #                  # Pass connections data to module
    #                  departure = args$departure,
    #                  arrival = args$arrival,
    #                  duration = args$duration,
    #                  transfers = args$transfers,
    #                  sections = sections
    #                )
    #              )
    #            }))
  }
