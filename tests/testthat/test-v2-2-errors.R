test_that("invalid input is rejected before calling the API", {

  # more than one pair
  expect_error(
    distance_single(origin = c("Prague", "Brno"),
                    destination = "Vienna",
                    key = "irrelevant"),
    "single distance pair is expected"
  )

  # unknown mode of transport
  expect_error(
    distance_single(origin = "Prague",
                    destination = "Vienna",
                    mode = "teleport",
                    key = "irrelevant"),
    "Mode of transportation not recognized"
  )

  # legacy mode of transport, as used by gmapsdistance()
  expect_error(
    distance_single(origin = "Prague",
                    destination = "Vienna",
                    mode = "walking",
                    key = "irrelevant"),
    "use mode = 'WALK' instead"
  )

})

test_that("place not found is reported, not silently dropped", {
  skip_on_cran() # because API key...

  not_found <- distance_single(
    origin = "King's Cross St. Pancras, London, UK",
    destination = "qwertzuiopasdfghjklyxcvbnm1234567890",
    key = Sys.getenv("GOOGLE_API_KEY")
  )

  expect_equal(not_found$status, "NOT_FOUND")

  # all elements present, but empty
  expect_named(not_found, c("status", "distance_text", "distance_value",
                            "duration_text", "duration_value"))
  expect_true(is.na(not_found$distance_value))
  expect_true(is.na(not_found$duration_value))

})

test_that("reserved characters in place names do not break the query", {
  skip_on_cran() # because API key...

  ampersand <- distance_single(
    origin = "Marks & Spencer, Oxford Street, London, UK",
    destination = "Victoria, London, UK",
    key = Sys.getenv("GOOGLE_API_KEY")
  )

  expect_equal(ampersand$status, "OK")
  expect_gt(ampersand$distance_value, 0)

})

test_that("coordinates are accepted as origin / destination", {
  skip_on_cran() # because API key...

  coords <- distance_single(
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
    distance_single(origin = "King's Cross St. Pancras, London, UK",
                    destination = "Victoria, London, UK",
                    key = "THIS-IS-NOT-A-VALID-KEY"),
    "API key not valid"
  )

})

test_that("single pair API call fails gracefully", {
  skip_on_cran() # because API key...
  skip_if(RCurl::url.exists("www.google.com"), "the internet is on") # othervise it would be no fun...

  expect_message(
    res <- distance_single(origin = "King's Cross St. Pancras, London, UK",
                           destination = "Victoria, London, UK",
                           key = Sys.getenv("GOOGLE_API_KEY"))
  )

  expect_true(is.na(res))

})
