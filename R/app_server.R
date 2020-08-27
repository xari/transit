#' @import shiny
app_server <- function(input, output,session) {
  trip_details <- mod_trip_selector_server("trip_selector")

  mod_details_wrapper_server(
			     "details_wrapper",
			     trip_details)

  # Dynamically render logos for the following packages.
  package_names <- c("shiny", "golem", "tidyverse")

  package_logos <-
    purrr::map(package_names,
               ~ {
                 # First attach a new "render" to the output object:
                 output[[.]] <-
                   renderImage(
                     list(
                       src = stringr::str_interp("img/${.}.png",),
                       contentType = "image/png",
                       alt = stringr::str_interp("${.} package logo"),
                       width = 100
                     ),
                     deleteFile = FALSE
                   )

                 # Then return the output, to be used in
                 # renderUI() below:
                 imageOutput(.,
                             height = "auto",
                             width = "auto")
               })

  output$footer_logos <-
    renderUI({
      div(
        class = "package_logos_wrapper",
        package_logos
      )
    })
}
