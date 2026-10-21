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
  expect_gt(walking$duration_value, driving$duration_value) # walking is slower than driving ...
  expect_lt(walking$distance_value, driving$distance_value) # ... but takes a more direct path

  expect_gt(bicycling$duration_value, transit$duration_value) # Tube is faster than both bike...
  expect_gt(driving$duration_value, transit$duration_value) # ... and driving (in London)

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
                        "duration_text", "duration_value"))
  expect_true(all(lengths(pairs) == 3))

  # one bad apple doesn't break the whole pipe
  expect_equal(pairs$status, c("OK", "OK", "NOT_FOUND"))
  expect_true(all(pairs$distance_value[1:2] > 0))
  expect_true(is.na(pairs$distance_value[3]))

})
