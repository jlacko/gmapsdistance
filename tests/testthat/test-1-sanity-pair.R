test_that("modes work", {
  skip_on_cran() # because API key...

  driving <- distance_single(
      origin = "King's Cross St. Pancras, London, UK",
      destination = "Victoria, London, UK",
      key = Sys.getenv("GOOGLE_API_KEY"),
      mode = "driving"
    )

  walking <- distance_single(
      origin = "King's Cross St. Pancras, London, UK",
      destination = "Victoria, London, UK",
      key = Sys.getenv("GOOGLE_API_KEY"),
      mode = "walking"
    )

  transit <- distance_single(
      origin = "King's Cross St. Pancras, London, UK",
      destination = "Victoria, London, UK",
      key = Sys.getenv("GOOGLE_API_KEY"),
      mode = "transit"
    )

  bicycling <- distance_single(
      origin = "King's Cross St. Pancras, London, UK",
      destination = "Victoria, London, UK",
      key = Sys.getenv("GOOGLE_API_KEY"),
      mode = "bicycling"
    )


  # http status - all modes should return a value
  expect_equal(driving$status, "OK")
  expect_equal(walking$status, "OK")
  expect_equal(transit$status, "OK")
  expect_equal(bicycling$status, "OK")

  # sanity check
  expect_gt(walking$duration_value, driving$duration_value) # walking is slower than driving ...
  expect_lt(walking$distance_value, driving$distance_value) # ... but takes a more direct path

  expect_gt(bicycling$duration_value, transit$duration_value) # Tube is faster than both bike...
  expect_gt(driving$duration_value, transit$duration_value) # ... and driving (in London)

})
