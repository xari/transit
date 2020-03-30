# Module UI

#' @title   mod_station_selector_ui and mod_station_selector_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_station_selector
#'
#' @keywords internal
#' @export
#' @importFrom shiny NS tagList

mod_station_selector_ui <-
  function(id,
           label = "Select a station.",
           placeholder = NULL) {
    ns <- NS(id)

    selectizeInput(
      ns("station"),
      label,
      choices = NULL,
      options = list(
        load = I(
          stringr::str_interp(
            "function(query, callback) {
if (!query.length) return callback();

let get_stations = new Promise((resolve, reject) => {
  Shiny.addCustomMessageHandler(
    '${ns('station')}',
    stations => {
      const stations_arr = [];

      let i;

      for(i = 0; i < 10; i++) {
        stations_arr.push({
          id: stations.value[i],
          name: stations.label[i]
        });
      }

      resolve(stations_arr);
    }
  )
});

get_stations.then(stations => {
      console.log(callback)

      callback(stations)
});

if (query.length > 0) {
  Shiny.setInputValue('${ns('station')}', query);
}
            }"
          )
        ),
        labelField = 'name',
        searchField = 'name',
        valueField = 'id',
        placeholder = placeholder
      )
    )
  }

# Module Server

#' @rdname mod_station_selector
#' @export
#' @keywords internal

mod_station_selector_server <-
  function(input, output, session) {
    ns <- session$ns

    observeEvent(
      input$station,
      ignoreInit = TRUE,
      {
        stations %>%
          dplyr::filter(stringr::str_detect(tolower(label), tolower(input$station))) %>%
          head(10) %>%
          print()

        session$sendCustomMessage(ns("station"),
                                  stations %>%
                                    dplyr::filter(stringr::str_detect(tolower(label), tolower(input$station))) %>%
                                    head(10))
      }
    )

    input
  }
