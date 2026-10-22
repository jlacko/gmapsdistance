
test_that("pairwise works", {
  skip_on_cran() # because API key...

  # 3 pairs: DC -> Miami, Miami -> Seattle, Seattle -> DC
  driving <- distance_routes(
    origin = c("Washington DC", "Miami FL", "Seattle WA"),
    destination = c("Miami FL", "Seattle WA", "Washington DC"),
    key = Sys.getenv("GOOGLE_API_KEY"),
    mode = "DRIVE"
  )

  # all cities found
  expect_true(all(driving$status == "OK"))

  # status = character vector
  expect_vector(driving$status, ptype = character())

  # pairwise = a data frame of 3 rows (and not 3x3 = 9)
  expect_named(driving, c("origin", "destination", "mode", "status",
                          "distance_text", "distance_value",
                          "duration_base", "duration_traffic"))
  expect_s3_class(driving, "data.frame")
  expect_equal(nrow(driving), 3)

  # origins, destinations and mode repeated as provided
  expect_equal(driving$origin, c("Washington DC", "Miami FL", "Seattle WA"))
  expect_equal(driving$destination, c("Miami FL", "Seattle WA", "Washington DC"))
  expect_equal(driving$mode, rep("DRIVE", 3))

  # results follow the order of the pairs
  expect_gt(driving$distance_value[2], driving$distance_value[3]) # Miami -> Seattle is the longest...
  expect_gt(driving$distance_value[3], driving$distance_value[1]) # ... and DC -> Miami the shortest

})

test_that("single pair returns a single row", {
  skip_on_cran() # because API key...

  driving <- distance_routes(
    origin = "Washington DC",
    destination = "Miami FL",
    key = Sys.getenv("GOOGLE_API_KEY")
  )

  expect_s3_class(driving, "data.frame")
  expect_equal(nrow(driving), 1)

})

test_that("one bad apple doesn't break whole pipe", {
  skip_on_cran() # because API key...

  # 3 pairs, the 2nd and 3rd with a place that can not be found
  driving <- distance_routes(
    origin = c("Washington DC", "psvz", "Seattle WA"),
    destination = c("Miami FL", "Seattle WA", "psvz"),
    key = Sys.getenv("GOOGLE_API_KEY")
  )

  # psvz place not found - neither as origin, nor as destination
  expect_equal(driving$status, c("OK", "NOT_FOUND", "NOT_FOUND"))

  # values for the good pair only
  expect_gt(driving$distance_value[1], 0)
  expect_true(all(is.na(driving$distance_value[2:3])))
  expect_true(all(is.na(driving$duration_base[2:3])))

})

test_that("identity pair has zero distance", {
  skip_on_cran() # because API key...

  # from a place to itself
  identity <- distance_routes(
    origin = c("Washington DC", "Seattle WA"),
    destination = c("Washington DC", "Miami FL"),
    key = Sys.getenv("GOOGLE_API_KEY")
  )

  expect_equal(identity$status, c("OK", "OK"))

  # zero, and not missing - Google omits zero values from its response
  expect_equal(identity$distance_value[1], 0)
  expect_equal(identity$duration_base[1], 0)

})

test_that("travel on public transport is faster at daytime", {
  skip_on_cran() # because API key...

  # the same trip twice, arriving at different times, New York time
  day <- as.character(Sys.Date() + 2)

  transit <- distance_routes(
    origin = rep("Washington DC", 2),
    destination = rep("New York City NY", 2),
    key = Sys.getenv("GOOGLE_API_KEY"),
    mode = "TRANSIT",
    arrival = as.POSIXct(paste(day, c("05:00:00",    # arrival at 5 AM = bad
                                      "17:00:00")),  # arrival at 5 PM = good
                         tz = "America/New_York")
  )

  expect_equal(transit$status, c("OK", "OK"))

  # night trip takes longer than day trip
  expect_lt(transit$duration_base[2], transit$duration_base[1])

})
