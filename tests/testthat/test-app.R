test_that("Connections are returned", {
  test_connection <- function(program) {
    from <- "Geneva"
    to <- "Basel"

    get_next_connections(from,
                         to,
                         date = "2020-02-28")
  }

  test_connection()
})
