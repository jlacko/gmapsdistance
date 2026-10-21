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

  # one value per pair, in every element
  expect_named(pairs, c("status", "distance_text", "distance_value",
                        "duration_base", "duration_traffic"))
  expect_true(all(lengths(pairs) == 3))

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
