test_that("modes work", {
  skip_on_cran() # because API key...

  driving <- distance_routes(
      origin = "King's Cross St. Pancras, London, UK",
      destination = "Victoria, London, UK",
      key = Sys.getenv("GOOGLE_API_KEY"),
      mode = "DRIVE"
    )

  walking <- distance_routes(
      origin = "King's Cross St. Pancras, London, UK",
      destination = "Victoria, London, UK",
      key = Sys.getenv("GOOGLE_API_KEY"),
      mode = "WALK"
    )

  transit <- distance_routes(
      origin = "King's Cross St. Pancras, London, UK",
      destination = "Victoria, London, UK",
      key = Sys.getenv("GOOGLE_API_KEY"),
      mode = "TRANSIT"
    )

  bicycling <- distance_routes(
      origin = "King's Cross St. Pancras, London, UK",
      destination = "Victoria, London, UK",
      key = Sys.getenv("GOOGLE_API_KEY"),
      mode = "BICYCLE"
    )

  truck <- distance_routes(
    origin = "King's Cross St. Pancras, London, UK",
    destination = "Victoria, London, UK",
    key = Sys.getenv("GOOGLE_API_KEY"),
    mode = "TRUCK"
  )

  two_wheeler <- distance_routes(
    origin = "King's Cross St. Pancras, London, UK",
    destination = "Victoria, London, UK",
    key = Sys.getenv("GOOGLE_API_KEY"),
    mode = "TWO_WHEELER"
  )

  # http status - all modes should return a value
  expect_equal(driving$status, "OK")
  expect_equal(walking$status, "OK")
  expect_equal(transit$status, "OK")
  expect_equal(bicycling$status, "OK")
  expect_equal(truck$status, "OK")

  # two wheelers are not routed in London (yet?) - no route is fine too
  expect_true(two_wheeler$status %in% c("OK", "ZERO_RESULTS"))

  # sanity check
  expect_gt(walking$duration_base, driving$duration_base) # walking is slower than driving ...
  expect_lt(walking$distance_value, driving$distance_value) # ... but takes a more direct path

  expect_gt(bicycling$duration_base, transit$duration_base) # Tube is faster than both bike...
  expect_gt(driving$duration_base, transit$duration_base) # ... and driving (in London)

})

test_that("pairwise distances work", {
  skip_on_cran() # because API key...

  pairs <- distance_routes(
    origin = c("King's Cross St. Pancras, London, UK",
               "Victoria, London, UK",
               "King's Cross St. Pancras, London, UK"),
    destination = c("Victoria, London, UK",
                    "King's Cross St. Pancras, London, UK",
                    "qwertzuiopasdfghjklyxcvbnm1234567890"),
    key = Sys.getenv("GOOGLE_API_KEY")
  )

  # one row per pair
  expect_named(pairs, c("origin", "destination", "mode", "status",
                        "distance_text", "distance_value",
                        "duration_base", "duration_traffic"))
  expect_s3_class(pairs, "data.frame")
  expect_equal(nrow(pairs), 3)

  # one bad apple doesn't break the whole pipe
  expect_equal(pairs$status, c("OK", "OK", "NOT_FOUND"))
  expect_true(all(pairs$distance_value[1:2] > 0))
  expect_true(is.na(pairs$distance_value[3]))

})

test_that("avoid works", {
  skip_on_cran() # because API key...

  plain <- distance_routes(
    origin = "Washington DC",
    destination = "New York City NY",
    key = Sys.getenv("GOOGLE_API_KEY")
  )

  # avoid tolls = big impact
  tolls <- distance_routes(
    origin = "Washington DC",
    destination = "New York City NY",
    key = Sys.getenv("GOOGLE_API_KEY"),
    avoid = "tolls"
  )

  # multiple features to avoid
  several <- distance_routes(
    origin = "Washington DC",
    destination = "New York City NY",
    key = Sys.getenv("GOOGLE_API_KEY"),
    avoid = c("tolls", "ferries")
  )

  # indoor navigation - walking only
  indoor <- distance_routes(
    origin = "King's Cross St. Pancras, London, UK",
    destination = "Victoria, London, UK",
    key = Sys.getenv("GOOGLE_API_KEY"),
    mode = "WALK",
    avoid = "indoor"
  )

  expect_equal(tolls$status, "OK")
  expect_equal(several$status, "OK")
  expect_equal(indoor$status, "OK")

  # no tolls = longer time...
  expect_gt(tolls$duration_base, plain$duration_base)

})

test_that("traffic model works", {
  skip_on_cran() # because API key...

  # no traffic model = both durations the same
  plain <- distance_routes(
    origin = "King's Cross St. Pancras, London, UK",
    destination = "Heathrow Airport, London, UK",
    key = Sys.getenv("GOOGLE_API_KEY")
  )

  pessimistic <- distance_routes(
    origin = "King's Cross St. Pancras, London, UK",
    destination = "Heathrow Airport, London, UK",
    key = Sys.getenv("GOOGLE_API_KEY"),
    traffic_model = "PESSIMISTIC"
  )

  optimistic <- distance_routes(
    origin = "King's Cross St. Pancras, London, UK",
    destination = "Heathrow Airport, London, UK",
    key = Sys.getenv("GOOGLE_API_KEY"),
    traffic_model = "OPTIMISTIC"
  )

  # traffic model with features to avoid
  best_guess <- distance_routes(
    origin = "King's Cross St. Pancras, London, UK",
    destination = "Heathrow Airport, London, UK",
    key = Sys.getenv("GOOGLE_API_KEY"),
    traffic_model = "BEST_GUESS",
    avoid = "tolls"
  )

  expect_equal(plain$duration_traffic, plain$duration_base)

  expect_equal(pessimistic$status, "OK")
  expect_equal(optimistic$status, "OK")
  expect_equal(best_guess$status, "OK")

  # bad traffic = longer time...
  expect_gte(pessimistic$duration_traffic, optimistic$duration_traffic)
  # gt would be better in principle, but in practice API _can_
  # return equal times for good & bad traffic (should not, but does...)

})

test_that("departure and arrival times work", {
  skip_on_cran() # because API key...

  # day after tomorrow, London time
  day <- as.character(Sys.Date() + 2)

  rush_hour <- distance_routes(
    origin = "King's Cross St. Pancras, London, UK",
    destination = "Heathrow Airport, London, UK",
    key = Sys.getenv("GOOGLE_API_KEY"),
    departure = as.POSIXct(paste(day, "08:00"), tz = "Europe/London")
  )

  night <- distance_routes(
    origin = "King's Cross St. Pancras, London, UK",
    destination = "Heathrow Airport, London, UK",
    key = Sys.getenv("GOOGLE_API_KEY"),
    departure = as.POSIXct(paste(day, "03:00"), tz = "Europe/London")
  )

  # public transport - arrival, and departure in the past
  arrival <- distance_routes(
    origin = "King's Cross St. Pancras, London, UK",
    destination = "Heathrow Airport, London, UK",
    key = Sys.getenv("GOOGLE_API_KEY"),
    mode = "TRANSIT",
    arrival = as.POSIXct(paste(day, "09:00"), tz = "Europe/London")
  )

  yesterday <- distance_routes(
    origin = "King's Cross St. Pancras, London, UK",
    destination = "Heathrow Airport, London, UK",
    key = Sys.getenv("GOOGLE_API_KEY"),
    mode = "TRANSIT",
    departure = Sys.time() - 86400
  )

  # one departure per pair - the same trip at two different times
  per_pair <- distance_routes(
    origin = rep("King's Cross St. Pancras, London, UK", 2),
    destination = rep("Heathrow Airport, London, UK", 2),
    key = Sys.getenv("GOOGLE_API_KEY"),
    departure = as.POSIXct(paste(day, c("08:00", "18:00")), tz = "Europe/London")
  )

  expect_equal(rush_hour$status, "OK")
  expect_equal(night$status, "OK")
  expect_equal(arrival$status, "OK")
  expect_equal(yesterday$status, "OK")
  expect_equal(per_pair$status, c("OK", "OK"))

  # rush hour = longer time in traffic than at night...
  expect_gte(rush_hour$duration_traffic, night$duration_traffic)

})

test_that("walking from Marathon to Athens works", {
  skip_on_cran() # because API key...

  # the vignette example, with coordinates comma separated
  marathon <- distance_routes(
    origin = "38.1621328,24.0029257",
    destination = "37.9908372,23.7383394",
    key = Sys.getenv("GOOGLE_API_KEY"),
    mode = "WALK"
  )

  expect_equal(marathon$status, "OK")

  # a marathon is 42.195 km, the shortest walk is a bit less
  expect_gt(marathon$distance_value, 35000)
  expect_lt(marathon$distance_value, 45000)

})

test_that("walking in Valparaíso works - negative coordinates", {
  skip_on_cran() # because API key...

  # from Plaza Sotomayor up the hill to La Sebastiana; southern & western hemisphere,
  # so both latitude and longitude are negative
  valparaiso <- distance_routes(
    origin = "-33.0391,-71.6283",
    destination = "-33.0463,-71.6165",
    key = Sys.getenv("GOOGLE_API_KEY"),
    mode = "WALK"
  )

  expect_equal(valparaiso$status, "OK")

  # a short walk across town - a lost minus sign would put the points
  # in a different hemisphere, with no walk between them at all
  expect_gt(valparaiso$distance_value, 1000)
  expect_lt(valparaiso$distance_value, 3000)

})
