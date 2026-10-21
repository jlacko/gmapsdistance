test_that("invalid input is rejected before calling the API", {

  # origins and destinations of different lengths - both ways
  expect_error(
    distance_routes(origin = c("Prague", "Brno"),
                    destination = "Vienna",
                    key = "irrelevant"),
    "Origin and destination must be of the same length"
  )

  expect_error(
    distance_routes(origin = "Prague",
                    destination = c("Vienna", "Bratislava"),
                    key = "irrelevant"),
    "Origin and destination must be of the same length"
  )

  # nothing to route
  expect_error(
    distance_routes(origin = character(0),
                    destination = character(0),
                    key = "irrelevant"),
    "At least one origin and one destination"
  )

  # more than one mode of transport
  expect_error(
    distance_routes(origin = "Prague",
                    destination = "Vienna",
                    mode = c("DRIVE", "WALK"),
                    key = "irrelevant"),
    "A single mode of transportation is expected"
  )

  # unknown mode of transport
  expect_error(
    distance_routes(origin = "Prague",
                    destination = "Vienna",
                    mode = "teleport",
                    key = "irrelevant"),
    "Mode of transportation not recognized"
  )

  # legacy mode of transport, as used by gmapsdistance()
  expect_error(
    distance_routes(origin = "Prague",
                    destination = "Vienna",
                    mode = "walking",
                    key = "irrelevant"),
    "use mode = 'WALK' instead"
  )

  # unknown feature to avoid
  expect_error(
    distance_routes(origin = "Prague",
                    destination = "Vienna",
                    avoid = "tunnels",
                    key = "irrelevant"),
    "Avoid parameters not recognized"
  )

  # road features to avoid while walking
  expect_error(
    distance_routes(origin = "Prague",
                    destination = "Vienna",
                    mode = "WALK",
                    avoid = "tolls",
                    key = "irrelevant"),
    "defined only for mode = 'DRIVE', 'TWO_WHEELER' or 'TRUCK'"
  )

  # indoor navigation to avoid while driving
  expect_error(
    distance_routes(origin = "Prague",
                    destination = "Vienna",
                    avoid = "indoor",
                    key = "irrelevant"),
    "defined only for mode = 'WALK'"
  )

  # indoor navigation to avoid on public transport
  expect_error(
    distance_routes(origin = "Prague",
                    destination = "Vienna",
                    mode = "TRANSIT",
                    avoid = "indoor",
                    key = "irrelevant"),
    "defined only for mode = 'WALK'"
  )

  # unknown traffic model
  expect_error(
    distance_routes(origin = "Prague",
                    destination = "Vienna",
                    traffic_model = "bflm",
                    key = "irrelevant"),
    "Traffic model not recognized"
  )

  # legacy traffic model, as used by gmapsdistance()
  expect_error(
    distance_routes(origin = "Prague",
                    destination = "Vienna",
                    traffic_model = "pessimistic",
                    key = "irrelevant"),
    "use traffic_model = 'PESSIMISTIC' instead"
  )

  expect_error(
    distance_routes(origin = "Prague",
                    destination = "Vienna",
                    traffic_model = "None",
                    key = "irrelevant"),
    "use traffic_model = NULL instead"
  )

  # more than one traffic model
  expect_error(
    distance_routes(origin = "Prague",
                    destination = "Vienna",
                    traffic_model = c("PESSIMISTIC", "OPTIMISTIC"),
                    key = "irrelevant"),
    "A single traffic model is expected"
  )

  # traffic model while walking
  expect_error(
    distance_routes(origin = "Prague",
                    destination = "Vienna",
                    mode = "WALK",
                    traffic_model = "PESSIMISTIC",
                    key = "irrelevant"),
    "Traffic models are defined only for mode = 'DRIVE'"
  )

  tomorrow <- Sys.time() + 86400

  # departure and arrival at the same time
  expect_error(
    distance_routes(origin = "Prague",
                    destination = "Vienna",
                    mode = "TRANSIT",
                    departure = tomorrow,
                    arrival = tomorrow,
                    key = "irrelevant"),
    "can not both be given"
  )

  # arrival by car
  expect_error(
    distance_routes(origin = "Prague",
                    destination = "Vienna",
                    arrival = tomorrow,
                    key = "irrelevant"),
    "Arrival times are defined only for mode = 'TRANSIT'"
  )

  # departure in the past, by car
  expect_error(
    distance_routes(origin = "Prague",
                    destination = "Vienna",
                    departure = Sys.time() - 86400,
                    key = "irrelevant"),
    "has to be some time in the future"
  )

  # departure as text, not date-time
  expect_error(
    distance_routes(origin = "Prague",
                    destination = "Vienna",
                    departure = "2026-11-02 08:00",
                    key = "irrelevant"),
    "should be a date-time \\(POSIXct\\)"
  )

  # neither one for all, nor one per pair
  expect_error(
    distance_routes(origin = c("Prague", "Brno", "Ostrava"),
                    destination = c("Vienna", "Bratislava", "Krakow"),
                    departure = tomorrow + c(0, 3600),
                    key = "irrelevant"),
    "got 2 for 3 pair\\(s\\)"
  )

  # missing departure
  expect_error(
    distance_routes(origin = "Prague",
                    destination = "Vienna",
                    departure = as.POSIXct(NA),
                    key = "irrelevant"),
    "can not be missing"
  )

})

test_that("place not found is reported, not silently dropped", {
  skip_on_cran() # because API key...

  not_found <- distance_routes(
    origin = "King's Cross St. Pancras, London, UK",
    destination = "qwertzuiopasdfghjklyxcvbnm1234567890",
    key = Sys.getenv("GOOGLE_API_KEY")
  )

  expect_equal(not_found$status, "NOT_FOUND")

  # all elements present, but empty
  expect_named(not_found, c("origin", "destination", "mode", "status",
                            "distance_text", "distance_value",
                            "duration_base", "duration_traffic"))
  expect_true(is.na(not_found$distance_value))
  expect_true(is.na(not_found$duration_base))

})

test_that("reserved characters in place names do not break the query", {
  skip_on_cran() # because API key...

  ampersand <- distance_routes(
    origin = "Marks & Spencer, Oxford Street, London, UK",
    destination = "Victoria, London, UK",
    key = Sys.getenv("GOOGLE_API_KEY")
  )

  expect_equal(ampersand$status, "OK")
  expect_gt(ampersand$distance_value, 0)

})

test_that("coordinates are accepted as origin / destination", {
  skip_on_cran() # because API key...

  coords <- distance_routes(
    origin = "51.5308,-0.1238", # King's Cross St. Pancras
    destination = "Victoria, London, UK",
    key = Sys.getenv("GOOGLE_API_KEY")
  )

  expect_equal(coords$status, "OK")
  expect_gt(coords$distance_value, 0)

})

test_that("an invalid API key raises Google's error", {
  skip_on_cran() # because internet...

  expect_error(
    distance_routes(origin = "King's Cross St. Pancras, London, UK",
                    destination = "Victoria, London, UK",
                    key = "THIS-IS-NOT-A-VALID-KEY"),
    "API key not valid"
  )

})
