test_that("modes work", {
  skip_on_cran() # because API key...

  driving <- distance_single(
      origin = "King's Cross St. Pancras, London, UK",
      destination = "Victoria, London, UK",
      key = Sys.getenv("GOOGLE_API_KEY"),
      mode = "DRIVE"
    )

  walking <- distance_single(
      origin = "King's Cross St. Pancras, London, UK",
      destination = "Victoria, London, UK",
      key = Sys.getenv("GOOGLE_API_KEY"),
      mode = "WALK"
    )

  transit <- distance_single(
      origin = "King's Cross St. Pancras, London, UK",
      destination = "Victoria, London, UK",
      key = Sys.getenv("GOOGLE_API_KEY"),
      mode = "TRANSIT"
    )

  bicycling <- distance_single(
      origin = "King's Cross St. Pancras, London, UK",
      destination = "Victoria, London, UK",
      key = Sys.getenv("GOOGLE_API_KEY"),
      mode = "BICYCLE"
    )

  truck <- distance_single(
    origin = "King's Cross St. Pancras, London, UK",
    destination = "Victoria, London, UK",
    key = Sys.getenv("GOOGLE_API_KEY"),
    mode = "TRUCK"
  )



  # http status - all modes should return a value
  expect_equal(driving$status, "OK")
  expect_equal(walking$status, "OK")
  expect_equal(transit$status, "OK")
  expect_equal(bicycling$status, "OK")
  expect_equal(truck$status, "OK")

  # sanity check
  expect_gt(walking$duration_value, driving$duration_value) # walking is slower than driving ...
  expect_lt(walking$distance_value, driving$distance_value) # ... but takes a more direct path

  expect_gt(bicycling$duration_value, transit$duration_value) # Tube is faster than both bike...
  expect_gt(driving$duration_value, transit$duration_value) # ... and driving (in London)

})

test_that("regional modes work", {
  skip_on_cran() # because API key...

  # trucks are routed in London ...

  # ... motorbikes in Jakarta (not in London though)
  two_wheeler <- distance_single(
      origin = "Monas, Jakarta",
      destination = "Kota Tua, Jakarta",
      key = Sys.getenv("GOOGLE_API_KEY"),
      mode = "TWO_WHEELER"
    )


  expect_equal(two_wheeler$status, "OK")

})
