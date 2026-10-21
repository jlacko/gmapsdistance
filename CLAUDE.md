# CLAUDE.md

R package `gmapsdistance` (on CRAN): distance and travel time between points via the
Google Maps Distance Matrix API. Maintainer: Jindra Lacko.

## Commands

```r
devtools::document()   # regenerate man/ and NAMESPACE from roxygen - never edit those by hand
devtools::test()       # run testthat suite (most tests hit the live API)
devtools::check()      # full R CMD check, as CI does
covr::package_coverage()
```

Tests need a real key in the `GOOGLE_API_KEY` environment variable (e.g. in `~/.Renviron`).
Every API call costs money, so avoid running the full suite in loops.

## Layout

- `R/gmapsdistance.R` - `gmapsdistance()`, the original exported function. Validates inputs,
  builds the request URL by hand, calls the **XML** endpoint via `RCurl::getURL`, parses with
  `XML`, loops over every origin/destination pair (one request each) and returns a list of
  `Distance`, `Time`, `Status` (scalars, wide matrices or long data frames).
- `R/distance_routes.R` - `distance_routes()`, newer pairwise function (origin[i] to
  destination[i], one request per pair) on the **Routes API** (`computeRouteMatrix`, JSON POST with `X-Goog-Api-Key` / `X-Goog-FieldMask`
  headers) via `httr::POST` + `jsonlite`. It maps Routes results back to the legacy
  `OK` / `NOT_FOUND` / `ZERO_RESULTS` statuses. `avoid` becomes `routeModifiers` on each
  origin (the matrix method rejects them at top level; it does not support `avoidTunnels`).
  `traffic_model` sets `routingPreference = TRAFFIC_AWARE_OPTIMAL` (Pro SKU, DRIVE only);
  results carry `duration_base` (`staticDuration`) and `duration_traffic` (`duration`).
  **Pairwise only, by design:** matrix output, `shape` and `combinations = "all"` are not
  planned for `distance_routes()` - don't add them or list them as gaps. Those stay with
  `gmapsdistance()`.
- `R/gmapsdistance-package.R` - package environment `pkg.env` holding the API key;
  `set.api.key()` (exported) / `get.api.key()` (internal, used as default for `key`).
- `vignettes/vignette.Rmd` is **pre-computed**: edit `vignette.Rmd.orig` and run
  `generate-vignette.R` (knits against the live API). Do not edit `vignette.Rmd` directly.

## Conventions

- Error handling contract, followed by both functions:
  - invalid user input -> `stop()` before any API call;
  - request-level API problems (bad key, quota) -> `stop()` with Google's status/message;
  - route-level problems (place not found, no route) -> reported in status, values `NA`;
  - API unreachable -> `message()` and return `NA` (CRAN policy: fail gracefully on
    internet resources). Those branches are wrapped in `# nocov start/end`.
- Let httr build query strings (`query = list(...)`) rather than pasting URLs - place names
  may contain `&`, `#`, `+`.
- Epoch seconds sent to the API must be formatted with `format(x, scientific = FALSE)`;
  R prints round numbers like `1.8e+09` otherwise.
- Only send optional parameters (departure time, mode, traffic model) when set - departure
  time with driving mode is billed at a higher rate.
- Style: base R, `<-` in new code (older code mixes in `=`), short lowercase comments.

## Tests

- testthat 3rd edition; `tests/testthat/test-v1-N-*.R` test `gmapsdistance()`,
  `tests/testthat/test-v2-N-*.R` test `distance_routes()`.
- Anything touching the API starts with `skip_on_cran()`; pure input-validation tests
  should not hit the API and need no skip.
- Prefer `expect_error(..., "pattern")` with the expected message over bare `expect_error()`.
- Offline-failure tests skip when the internet is up; mocking (`local_mocked_bindings`)
  is the way to exercise them deterministically.

## Release

- Bump `Version` and `Date` in DESCRIPTION, add an entry to `NEWS.md`, update
  `cran-comments.md`.
- CI: `.github/workflows/check-standard.yaml` (R CMD check on Windows/macOS release and
  Ubuntu devel, coverage to codecov); the key comes from the `GOOGLE_API_KEY` secret.
- New top-level files must be added to `.Rbuildignore`.
