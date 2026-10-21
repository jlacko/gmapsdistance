#' Compute Distance for a Single Pair with Google Maps
#'
#' The function distance_single uses the Compute Route Matrix method of the
#' Google Maps Routes API to compute the distance and travel time between a
#' single origin and a single destination. In order to be able to use the
#' function you will need an API key and enable the Routes API in the Google
#' Developers Console. For more information about how to get a key, go to
#' https://developers.google.com/maps/documentation/routes/get-api-key
#' For more information about the Google Maps Routes API go to
#' https://developers.google.com/maps/documentation/routes/compute_route_matrix
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
#' @param mode A string containing the mode of transportation desired, using
#'  the travel mode names of the Routes API. One of "DRIVE" (default), "WALK",
#'  "BICYCLE", "TRANSIT", "TWO_WHEELER" or "TRUCK".
#'
#'  Note that "TWO_WHEELER" (motorized two wheelers) and "TRUCK" are supported
#'  only in some regions; elsewhere the status is "ZERO_RESULTS".
#'
#' @param key Google Maps API key. Defaults to the key set by
#'  \code{set.api.key("THISISMYKEY")}.
#'
#' @return A list with 5 named elements, or \code{NA} if the API could not be
#'  reached:
#' \itemize{
#'    \item{\strong{status}: route status; "OK", "NOT_FOUND" (origin or
#'    destination could not be geocoded) or "ZERO_RESULTS" (no route between
#'    the points). Note that these differ from the status values of
#'    \code{gmapsdistance()}.}
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
#'                 mode = "WALK")
#' }
#'
#' @export
#'
#' @importFrom httr content
#' @importFrom httr POST
#' @importFrom jsonlite fromJSON

distance_single <- function(origin,
                            destination,
                            mode = "DRIVE",
                            key = get.api.key()
                            ) {

  # input validation - length
  if (!all(c(length(origin) == 1, length(destination) == 1, length(mode) == 1))) {

    stop("single distance pair is expected")

  }

  # input validation - mode of transport
  if (!mode %in% c("DRIVE", "WALK", "BICYCLE", "TRANSIT", "TWO_WHEELER", "TRUCK")) {

    stop("Mode of transportation not recognized. Mode should be one of ",
         "'DRIVE', 'WALK', 'BICYCLE', 'TRANSIT', 'TWO_WHEELER', 'TRUCK'")

  }

  # Routes API does not geocode "lat,long" strings; coordinates go as latLng instead
  waypoints <- lapply(c(origin, destination), \(x) {
    coords <- regmatches(x, regexec("^\\s*(-?[0-9.]+)\\s*,\\s*(-?[0-9.]+)\\s*$", x))[[1]]
    if (length(coords) == 3) {
      list(waypoint = list(location = list(latLng = list(latitude = as.numeric(coords[2]),
                                                         longitude = as.numeric(coords[3])))))
    } else {
      list(waypoint = list(address = x))
    }
  })

  # make the call! the Routes API expects a POST with a JSON body; the key and
  # the list of fields to return (mandatory) travel in the headers
  resp <- tryCatch(
    httr::POST("https://routes.googleapis.com/distanceMatrix/v2:computeRouteMatrix",
               httr::add_headers(
                 `X-Goog-Api-Key` = gsub(" ", "", key),
                 `X-Goog-FieldMask` = paste("status", "condition", "distanceMeters",
                                            "duration", "localizedValues", sep = ",")
               ),
               body = list(origins = waypoints[1],
                           destinations = waypoints[2],
                           travelMode = mode,
                           units = "METRIC"),
               encode = "json"),
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

  # parse the response once; the REST endpoint returns an array of elements
  body <- jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"),
                             simplifyVector = FALSE)

  # request level errors - reported via HTTP status, details in the body
  if (httr::http_error(resp)) {

    error <- if (is.null(body$error)) body[[1]]$error else body$error

    stop("Google API returned an error: ", error$status,
         if (!is.null(error$message)) paste0(" - ", error$message))

  }

  # route level result - the single element for the single pair
  element <- body[[1]]

  # element level errors; "not found" is a route level problem, the rest are not
  if (!is.null(element$status$code)) {

    if (element$status$code != 5) {

      stop("Google API returned an error: ", element$status$message)

    }

    status <- "NOT_FOUND"

  } else if (identical(element$condition, "ROUTE_EXISTS")) {

    status <- "OK"

  } else {

    status <- "ZERO_RESULTS"

  }

  # compose the result object; missing values (e.g. place not found, no route) become NA
  list(
    status = status,
    distance_text = (\(x) if (is.null(x)) NA else x)(element$localizedValues$distance$text),
    distance_value = (\(x) if (is.null(x)) NA else x)(element$distanceMeters),
    duration_text = (\(x) if (is.null(x)) NA else x)(element$localizedValues$duration$text),
    duration_value = (\(x) if (is.null(x)) NA else as.numeric(sub("s$", "", x)))(element$duration)
  )

}
