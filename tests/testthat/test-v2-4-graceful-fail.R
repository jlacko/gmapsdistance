# meant to be run manually, from an offline computer (together with test-v1-6-graceful-fail.R):
# testthat::test_file("tests/testthat/test-v2-4-graceful-fail.R")

test_that("API call fails gracefully", {
  skip_on_cran() # because API key...
  skip_if(RCurl::url.exists("www.google.com"), "the internet is on") # othervise it would be no fun...

  expect_message( # i.e. do not expect an error!!!
    res <- distance_routes(origin = "King's Cross St. Pancras, London, UK",
                           destination = "Victoria, London, UK",
                           key = Sys.getenv("GOOGLE_API_KEY")),
    "Google API call failed; check your internet connection"
  )

  # fail, but graceful
  expect_true(is.na(res))

})

test_that("API call for multiple pairs fails gracefully", {
  skip_on_cran() # because API key...
  skip_if(RCurl::url.exists("www.google.com"), "the internet is on") # othervise it would be no fun...

  expect_message( # i.e. do not expect an error!!!
    res <- distance_routes(origin = c("King's Cross St. Pancras, London, UK",
                                      "Victoria, London, UK"),
                           destination = c("Victoria, London, UK",
                                           "Heathrow Airport, London, UK"),
                           key = Sys.getenv("GOOGLE_API_KEY")),
    "Google API call failed; check your internet connection"
  )

  # a single NA for the whole call, not one per pair
  expect_identical(res, NA)

})
