#' Compute Distance for a Single Pair with Google Maps
#'
#' The function distance_single uses the JSON interface of the Google Maps
#' Distance Matrix API to compute the distance and travel time between a single
#' origin and a single destination. In order to be able to use the function you
#' will need an API key and enable the Distance Matrix API in the Google
#' Developers Console. For more information about how to get a key, go to
#' https://developers.google.com/maps/documentation/distance-matrix/get-api-key#key
#' For more information about the Google Maps Distance Matrix API go to
#' https://developers.google.com/maps/documentation/distance-matrix/intro?hl=en
#'
#' Request level problems (such as an invalid API key or an exhausted quota)
#' raise an error carrying the message returned by Google. Route level problems
#' (an origin or destination that can not be geocoded, or no route between the
#' two) are reported in the \code{status} element, with distance and duration
#' set to \code{NA}. If the API can not be reached at all the function fails
#' gracefully: it emits a message and returns \code{NA}.
#'
#' @param origin A single string containing the description of the starting
#'  point. Coordinates in LAT-LONG format (e.g. "50.087,14.421") are also a
#'  valid input as long as they can be identified by Google Maps.
#'
#' @param destination A single string containing the description of the end
#'  point. Should be the same format as the variable "origin".
#'
#' @param mode A string containing the mode of transportation desired. One of
#'  "driving" (default), "walking", "bicycling" or "transit".
#'
#' @param key Google Maps API key. Defaults to the key set by
#'  \code{set.api.key("THISISMYKEY")}.
#'
#' @return A list with 5 named elements, or \code{NA} if the API could not be
#'  reached:
#' \itemize{
#'    \item{\strong{status}: element status as returned by Google; "OK",
#'    "NOT_FOUND" (origin or destination could not be geocoded) or
#'    "ZERO_RESULTS" (no route between the points). Note that these differ from
#'    the status values of \code{gmapsdistance()}.}
#'    \item{\strong{distance_text}: human readable distance, e.g. "4.2 km".}
#'    \item{\strong{distance_value}: distance in meters; \code{NA} when status is
#'    not "OK".}
#'    \item{\strong{duration_text}: human readable travel time, e.g. "17 mins".}
#'    \item{\strong{duration_value}: travel time in seconds; \code{NA} when
#'    status is not "OK".}
#'  }
#'
#' @seealso \code{\link{gmapsdistance}} for multiple origins / destinations,
#'  departure and arrival times, traffic models and route restrictions.
#'
#' @examples
#' \dontshow{set.api.key(Sys.getenv("GOOGLE_API_KEY"))}
#' \dontrun{
#' # walking distance between two London stations
#' distance_single(origin = "King's Cross St. Pancras, London, UK",
#'                 destination = "Victoria, London, UK",
#'                 mode = "walking")
#' }
#'
#' @export
#'
#' @importFrom httr content
#' @importFrom httr GET
#' @importFrom jsonlite fromJSON

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

  # compose the result object; missing values (e.g. place not found, no route) become NA
  list(
    status = element$status,
    distance_text = (\(x) if (is.null(x)) NA else x)(element$distance$text),
    distance_value = (\(x) if (is.null(x)) NA else x)(element$distance$value),
    duration_text = (\(x) if (is.null(x)) NA else x)(element$duration$text),
    duration_value = (\(x) if (is.null(x)) NA else x)(element$duration$value)
  )

}
