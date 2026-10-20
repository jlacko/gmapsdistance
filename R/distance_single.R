#' Baseline scenario - pairwise origin / destination call
#'
#' Computes distance and travel time for a single origin / destination pair
#' using the JSON interface of the Google Maps Distance Matrix API.
#'
#' Request level problems (such as an invalid API key or an exhausted quota)
#' raise an error carrying the message returned by Google. Route level problems
#' (an origin or destination that can not be geocoded, or no route between the
#' two) are reported in the \code{status} element, with distance and duration
#' set to \code{NA}. If the API can not be reached at all the function fails
#' gracefully: it emits a message and returns \code{NA}.
#'
#' @param origin a single origin
#' @param destination a single destination
#' @param mode mode of transport; one of "driving", "walking", "bicycling" or
#'   "transit"
#' @param key Google API key
#'
#' @returns a list of values: status, distance text + value (meters), duration
#'   text + value (seconds); \code{NA} if the API could not be reached
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

  # input validation - length
  if (!all(c(length(origin) == 1, length(destination) == 1, length(mode) == 1))) {

    stop("single distance pair is expected")

  }

  # input validation - mode of transport
  if (!mode %in% c("driving", "walking", "bicycling", "transit")) {

    stop("Mode of transportation not recognized. Mode should be one of ",
         "'bicycling', 'transit', 'driving', 'walking'")

  }

  # make the call! httr builds & encodes the query string, so that reserved
  # characters in place names (such as & or #) do not break it
  resp <- tryCatch(
    httr::GET("https://maps.googleapis.com/maps/api/distancematrix/json",
              query = list(origins = origin,
                           destinations = destination,
                           mode = mode,
                           units = "metric",
                           key = gsub(" ", "", key))),
    # nocov start
    error = function(e) NULL
    # nocov end
  )

  # if the API could not be reached >> fail the function, but gracefully
  if (is.null(resp)) {

    # nocov start
    message("Google API call failed; check your internet connection")
    return(NA)
    # nocov end

  }

  # HTTP level errors
  httr::stop_for_status(resp)

  # parse the response once
  body <- jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"))

  # request level errors - Google returns these with HTTP 200
  if (!identical(body$status, "OK")) {

    stop("Google API returned an error: ", body$status,
         if (!is.null(body$error_message)) paste0(" - ", body$error_message))

  }

  # route level result - the single element of the single row
  element <- body$rows$elements[[1]]

  # missing values (e.g. place not found, no route) become NA
  na_if_null <- function(x) if (is.null(x)) NA else x

  # compose the result object
  list(
    status = element$status,
    distance_text = na_if_null(element$distance$text),
    distance_value = na_if_null(element$distance$value),
    duration_text = na_if_null(element$duration$text),
    duration_value = na_if_null(element$duration$value)
  )

}
