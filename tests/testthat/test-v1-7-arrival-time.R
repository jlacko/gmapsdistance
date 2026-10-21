
test_that("travel on public transport is faster at daytime", {
  skip_on_cran() # because API key...

  skip("the assumption behind this test is too strong; skipping for now")

  night <- gmapsdistance(
    origin = "Washington DC",
    destination = "New York City NY",
    key = Sys.getenv("GOOGLE_API_KEY"),
    arr_date = as.character(Sys.Date() + 2),
    arr_time = "05:00:00", # arrival at 5 AM = bad
    mode = "transit"
  )

  day <- gmapsdistance(
    origin = "Washington DC",
    destination = "New York City NY",
    key = Sys.getenv("GOOGLE_API_KEY"),
    arr_date = as.character(Sys.Date() + 2),
    arr_time = "17:00:00", # arrival at 5 PM = good
    mode = "transit"
  )


 expect_lt(day$Time, night$Time)

})
