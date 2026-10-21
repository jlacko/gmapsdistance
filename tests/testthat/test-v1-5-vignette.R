test_that("pesky troublemaker", {
  skip_on_cran() # because API key...

  results <- gmapsdistance(origin = c("Washington DC", "New York NY",
                                      "Seattle WA", "Miami FL"),
                           destination = c("Los Angeles CA", "Austin TX",
                                           "Chicago IL", "Philadelphia PA"),
                           mode = "bicycling",
                           dep_date = as.character(Sys.Date() + 2),
                           dep_time = "22:20:00",
                           key = Sys.getenv("GOOGLE_API_KEY"))

  expect_equal(unique(as.vector(results$Status)), "OK")
  expect_equal(any(is.na(as.vector(results$Time))), FALSE)

  karel <- paste(as.character(Sys.Date() + 2), "22:20:00 CET") # date as text

  results <- gmapsdistance(origin = c("Washington DC", "New York NY",
                                      "Seattle WA", "Miami FL"),
                           destination = c("Los Angeles CA", "Austin TX",
                                           "Chicago IL", "Philadelphia PA"),
                           mode = "bicycling",
                           departure = as.numeric(as.POSIXct(karel)), # text as UNIX timestamp
                           key = Sys.getenv("GOOGLE_API_KEY"))

  expect_equal(unique(as.vector(results$Status)), "OK")
  expect_equal(any(is.na(as.vector(results$Time))), FALSE)

})
