#' @import shiny
app_server <- function(input, output,session) {
  # This module renders the form fields whose values will be
  # used to query the API.
  # It returns a value, which is a list that contains the value
  # of each form field.
  trip_details <- mod_trip_selector_server("trip_selector")

  # This module renders the buttons for selecting the
  # journey and the tab panel for visualizing the GT
  # and the map.
  mod_details_wrapper_server("details_wrapper", trip_details)

  # Dynamically render logos for the following packages.
  package_names <- c("shiny", "golem", "tidyverse")

  # This variable creates a vector of images.
  package_logos <-
    purrr::map(package_names,
               ~ {
                 # In order to do this, we need to first attach a new "render"
		 # to the output object.
		 # We can access the output object directly, and assign a new
		 # property to it for each of the package names.
		 # That way; if we ever had more package names that we wanted
		 # to include, we'd simply have to update the package_names
		 # vector above.
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
		 # Normally, imageOutput() is a function that we use in the UI portion
		 # of our modules or app, but we can use it inside of our Server, if we
		 # use it inside of a call to renderUI().
		 # This technique can enable us to make our UI truely dynamic.
               })

  # Wrap the logo images in a div and render them.
  output$footer_logos <-
    renderUI({
      div(
        class = "package_logos_wrapper",
        package_logos
      )
    })
}
