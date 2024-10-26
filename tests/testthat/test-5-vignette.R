test_that("pesky troublemaker", {
  skip_on_cran() # because API key...

  results <- gmapsdistance(origin = c("Washington DC", "New York NY",
                                      "Seattle WA", "Miami FL"),
                           destination = c("Los Angeles CA", "Austin TX",
                                           "Chicago IL", "Philadelphia PA"),
                           mode = "bicycling",
                           dep_date = "2025-02-19",
                           dep_time = "22:20:00",
                           key = Sys.getenv("GOOGLE_API_KEY"))

  expect_equal(unique(as.vector(results$Status)), "OK")
  expect_equal(any(is.na(as.vector(results$Time))), FALSE)


  results <- gmapsdistance(origin = c("Washington DC", "New York NY",
                                      "Seattle WA", "Miami FL"),
                           destination = c("Los Angeles CA", "Austin TX",
                                           "Chicago IL", "Philadelphia PA"),
                           mode = "bicycling",
                           # departure time as seconds from Unix Epoch (1970-01-01)
                           # check that it happens in future: as.POSIXct(1710000000, origin="1970-01-01")
                           departure = 1740000000,
                           key = Sys.getenv("GOOGLE_API_KEY"))

  expect_equal(unique(as.vector(results$Status)), "OK")
  expect_equal(any(is.na(as.vector(results$Time))), FALSE)

})
