#' Baseline scenario - pairvise origin / destination call
#'
#' @param origin a single origin
#' @param destination a single destination
#' @param mode mode of tranport
#' @param key Google API key
#'
#' @returns a list of values: status, duration text + value, distance text + value
#' @export
#'
#' @importFrom httr content
#' @importFrom httr GET
#' @importFrom jsonlite fromJSON
#' @importFrom magrittr %>%
#' @importFrom magrittr extract2

distance_single <- function(origin,
                            destination,
                            mode = "driving",
                            key = get.api.key()
                            ) {
  # result object construction
  result <- list()


  # input validation - length
  if (!all(c(length(origin) == 1, length(destination) ==1, length(mode) == 1))) {

    stop("single distance pair is expected")

  }

  # query string construction
  query <- paste0("https://maps.googleapis.com/maps/api/distancematrix/json",
                  "?destinations=",
                  destination,
                  "&origins=",
                  origin,
                  "&mode=",
                  mode,
                  "&key=",
                  key)

  # get rid of the funny characters
  query <- utils::URLencode(query)

  # make the call!
  resp <- httr::GET(query)
  # wait for response
  httr::stop_for_status(resp)

  # digest geocoding status
  result$status <- httr::content(resp, as = "text") %>%
    jsonlite::fromJSON() %>%
    magrittr::extract2("status")

  # digest geocoding result
  asdf <- httr::content(resp, as = "text") %>%
    jsonlite::fromJSON() %>%
    magrittr::extract2("rows")

  # compose the result object
  result$distance_text <- asdf$elements[[1]]$distance$text
  result$distance_value <- asdf$elements[[1]]$distance$value
  result$duration_text <- asdf$elements[[1]]$duration$text
  result$duration_value <- asdf$elements[[1]]$duration$value

  # return the result object
  result

}
