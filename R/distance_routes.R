#' Compute Pairwise Distances with Google Maps
#'
#' The function distance_routes uses the Compute Route Matrix method of the
#' Google Maps Routes API to compute the distance and travel time between
#' pairs of points: the first origin to the first destination, the second
#' origin to the second destination and so on. In order to be able to use the
#' function you will need an API key and enable the Routes API in the Google
#' Developers Console. For more information about how to get a key, go to
#' https://developers.google.com/maps/documentation/routes/get-api-key
#' For more information about the Google Maps Routes API go to
#' https://developers.google.com/maps/documentation/routes/compute_route_matrix
#'
#' Request level problems (such as an invalid API key or an exhausted quota)
#' raise an error carrying the message returned by Google. Route level problems
#' (an origin or destination that can not be geocoded, or no route between the
#' two) are reported per pair in the \code{status} element, with distance and
#' duration set to \code{NA}. If the API can not be reached at all the function
#' fails gracefully: it emits a message and returns \code{NA}.
#'
#' @param origin A string or vector of strings containing the description of
#'  the starting point(s). Coordinates in LAT-LONG format (e.g. "50.087,14.421")
#'  are also a valid input as long as they can be identified by Google Maps.
#'
#' @param destination A string or vector of strings containing the description
#'  of the end point(s). Should be the same format as the variable "origin".
#'
#'  Origins and destinations are paired, so the two vectors must be of the
#'  same length; a difference in lengths is an error.
#'
#' @param mode A string containing the mode of transportation desired, using
#'  the travel mode names of the Routes API. One of "DRIVE" (default), "WALK",
#'  "BICYCLE", "TRANSIT", "TWO_WHEELER" or "TRUCK".
#'
#'  Note that "TWO_WHEELER" (motorized two wheelers) and "TRUCK" are supported
#'  only in some regions; elsewhere the status is "ZERO_RESULTS".
#'
#'  Note also that these values differ from the modes of transportation of
#'  \code{gmapsdistance()} ("driving", "walking", "bicycling", "transit"), due to
#'  the move from the legacy Distance Matrix API to the Routes API. The legacy
#'  values are rejected with an error pointing to their Routes API equivalent.
#'
#' @param key Google Maps API key. Defaults to the key set by
#'  \code{set.api.key("THISISMYKEY")}.
#'
#' @param avoid A string or vector of strings with route features to avoid,
#'  applied to all pairs. Any of "tolls", "highways" and "ferries" (for mode
#'  "DRIVE", "TWO_WHEELER" or "TRUCK") or "indoor" (for mode "WALK"); they
#'  correspond to the \code{avoidTolls}, \code{avoidHighways},
#'  \code{avoidFerries} and \code{avoidIndoor} route modifiers of the Routes API.
#'  Default is \code{NULL}, i.e. nothing is avoided.
#'
#'  Avoiding is a preference, not a guarantee: Google avoids the features where
#'  reasonable, so a route may still include them.
#'
#' @return A list with 5 named elements, each a vector with one value per
#'  origin / destination pair, or \code{NA} if the API could not be reached:
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
#' @seealso \code{\link{gmapsdistance}} for all combinations of origins and
#'  destinations, departure and arrival times and traffic models.
#'
#' @examples
#' \dontshow{set.api.key(Sys.getenv("GOOGLE_API_KEY"))}
#' \dontrun{
#' # walking distance between two London stations
#' distance_routes(origin = "King's Cross St. Pancras, London, UK",
#'                 destination = "Victoria, London, UK",
#'                 mode = "WALK")
#'
#' # driving distances for two pairs: London to Paris and Prague to Vienna
#' distance_routes(origin = c("London, UK", "Prague, CZ"),
#'                 destination = c("Paris, FR", "Vienna, AT"))
#'
#' # driving distance from Washington DC to NYC, avoiding toll roads
#' distance_routes(origin = "Washington DC",
#'                 destination = "New York City NY",
#'                 avoid = "tolls")
#' }
#'
#' @export
#'
#' @importFrom httr content
#' @importFrom httr POST
#' @importFrom jsonlite fromJSON

distance_routes <- function(origin,
                            destination,
                            mode = "DRIVE",
                            key = get.api.key(),
                            avoid = NULL
                            ) {

  # input validation - length
  if (length(mode) != 1) {

    stop("A single mode of transportation is expected.")

  }

  if (length(origin) == 0 || length(destination) == 0) {

    stop("At least one origin and one destination are expected.")

  }

  # origins and destinations are paired, so they must match one to one
  if (length(origin) != length(destination)) {

    stop("Origin and destination must be of the same length; got ",
         length(origin), " origin(s) and ", length(destination), " destination(s).")

  }

  # input validation - legacy modes of transport, as used by gmapsdistance()
  legacy_modes <- c(driving = "DRIVE", walking = "WALK",
                    bicycling = "BICYCLE", transit = "TRANSIT")

  if (mode %in% names(legacy_modes)) {

    stop("Mode '", mode, "' is a legacy Distance Matrix API value, as used by ",
         "gmapsdistance(). distance_routes() uses the Routes API travel modes; ",
         "use mode = '", legacy_modes[[mode]], "' instead.")

  }

  # input validation - mode of transport
  if (!mode %in% c("DRIVE", "WALK", "BICYCLE", "TRANSIT", "TWO_WHEELER", "TRUCK")) {

    stop("Mode of transportation not recognized. Mode should be one of ",
         "'DRIVE', 'WALK', 'BICYCLE', 'TRANSIT', 'TWO_WHEELER', 'TRUCK'")

  }

  # input validation - route features to avoid, named as Routes API route modifiers
  avoid <- unique(avoid)
  avoid_modifiers <- c(tolls = "avoidTolls", highways = "avoidHighways",
                       ferries = "avoidFerries", indoor = "avoidIndoor")

  if (!all(avoid %in% names(avoid_modifiers))) {

    stop("Avoid parameters not recognized. Avoid should be one of ",
         "'tolls', 'highways', 'ferries', 'indoor'")

  }

  if (any(avoid %in% c("tolls", "highways", "ferries")) &&
        !mode %in% c("DRIVE", "TWO_WHEELER", "TRUCK")) {

    stop("Avoiding 'tolls', 'highways' and 'ferries' is defined only for ",
         "mode = 'DRIVE', 'TWO_WHEELER' or 'TRUCK'.")

  }

  if ("indoor" %in% avoid && mode != "WALK") {

    stop("Avoiding 'indoor' is defined only for mode = 'WALK'.")

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

  n <- length(origin)
  results <- vector("list", n)

  # features to avoid travel as route modifiers of each origin
  if (length(avoid) > 0) {

    route_modifiers <- as.list(rep(TRUE, length(avoid)))
    names(route_modifiers) <- unname(avoid_modifiers[avoid])

    waypoints[seq_len(n)] <- lapply(waypoints[seq_len(n)],
                                    \(x) c(x, list(routeModifiers = route_modifiers)))

  }

  # one call per pair - a single matrix call would be billed for all n x n elements
  for (i in seq_len(n)) {

    # make the call! the Routes API expects a POST with a JSON body; the key and
    # the list of fields to return (mandatory) travel in the headers
    resp <- tryCatch(
      httr::POST("https://routes.googleapis.com/distanceMatrix/v2:computeRouteMatrix",
                 httr::add_headers(
                   `X-Goog-Api-Key` = gsub(" ", "", key),
                   `X-Goog-FieldMask` = paste("status", "condition", "distanceMeters",
                                              "duration", "localizedValues", sep = ",")
                 ),
                 body = list(origins = waypoints[i],
                             destinations = waypoints[n + i],
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

    # route level result - the single element for the current pair
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

    # Google omits zero values from its response (e.g. from a place to itself), so a
    # missing value of a found route is zero; of a route not found it is NA
    missing_value <- if (status == "OK") 0 else NA

    # result for the current pair; missing texts (e.g. place not found, no route) become NA
    results[[i]] <- list(
      status = status,
      distance_text = (\(x) if (is.null(x)) NA else x)(element$localizedValues$distance$text),
      distance_value = (\(x) as.numeric(if (is.null(x)) missing_value else x))(element$distanceMeters),
      duration_text = (\(x) if (is.null(x)) NA else x)(element$localizedValues$duration$text),
      duration_value = (\(x) if (is.null(x)) as.numeric(missing_value) else as.numeric(sub("s$", "", x)))(element$duration)
    )

  } # / end for cycle

  # compose the result object - one vector per field, one value per pair
  sapply(names(results[[1]]),
         \(field) unlist(lapply(results, \(pair) pair[[field]])),
         simplify = FALSE)

}
