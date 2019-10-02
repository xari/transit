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

  uiOutput(ns("connections"))
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

    output$connections <-
      renderUI({
        get_next_departures(origin$station, destination$station) %>% # Returns a list of connections
          tibble::rowid_to_column() %>% # Creates rowid column
          purrr::pmap(function(...) {
            args <- list(...) # The list of connection details

            # Create a unique ID for each module instance
            module_unique_id <-
              stringr::str_c("connection_", args$rowid)

            # Create module UIs using unique ID
            mod_connection_table_ui(module_unique_id)

            # Call modules using unique ID
            callModule(mod_connection_table_server,
                       module_unique_id,
                       getConnectionTable(args$sections))
          })
      })
  }
