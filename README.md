gmapsdistance
=======
[![R-CMD-check](https://github.com/jlacko/gmapsdistance/workflows/R-CMD-check/badge.svg)](https://github.com/jlacko/gmapsdistance/actions)
![Downloads-monthly](https://cranlogs.r-pkg.org/badges/gmapsdistance?color=brightgreen)
![Downloads-total](https://cranlogs.r-pkg.org/badges/grand-total/gmapsdistance?color=brightgreen)
![License](https://img.shields.io/badge/license-GPL--3-brightgreen.svg?style=flat)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/gmapsdistance)](https://cran.r-project.org/package=gmapsdistance)
[![CRAN-checks](https://badges.cranchecks.info/summary/gmapsdistance.svg)](https://cran.r-project.org/web/checks/check_results_gmapsdistance.html)
[![Codecov test coverage](https://codecov.io/gh/jlacko/gmapsdistance/branch/master/graph/badge.svg)](https://app.codecov.io/gh/jlacko/gmapsdistance?branch=master)

# Interface Between R and Google Maps

The package computes distances and travel times between points using Google Maps, with two functions:

- `distance_routes()` uses the [Routes API](https://developers.google.com/maps/documentation/routes), Google's current API for this purpose. It computes distances and times for pairs of points (first origin to first destination, second to second, and so on) and returns a data frame with one row per pair. Modes of transportation are `DRIVE`, `WALK`, `BICYCLE`, `TRANSIT`, `TWO_WHEELER` and `TRUCK`.
- `gmapsdistance()` uses the [Distance Matrix API](https://developers.google.com/maps/documentation/distance-matrix/overview), and can also compute full matrices of all origins against all destinations. The Distance Matrix API is now legacy: since October 2026 it can no longer be enabled for new Google Cloud projects. `gmapsdistance()` is kept for backward compatibility.

Distances are returned in meters and times in seconds.

An [API key](https://developers.google.com/maps/documentation/routes/get-api-key) is necessary to perform the queries, with the Routes API (for `distance_routes()`) or the Distance Matrix API (for `gmapsdistance()`) enabled. Google Maps must be able to find both the origin and the destination of a trip.

While the R package is open source, the Google Maps APIs are a commercial service requiring registration in all cases. Google provides a free monthly allowance - e.g. 10,000 basic Routes API requests (each origin - destination pair counts as one) - which is sufficient for most R package users; requests considering traffic, and those for trucks and two-wheelers, have smaller allowances. See [Google Maps Platform pricing](https://developers.google.com/maps/billing-and-pricing/pricing) for details.

Also note that using the API is subject to [Google Maps Platform Terms of Service](https://cloud.google.com/maps-platform/terms).


## Installation

```r
# CRAN install / stable version 
install.packages("gmapsdistance")

# Github installation / current dev version
remotes::install_github("jlacko/gmapsdistance")
```

## Examples of use
In this example we compute the driving distance between Washington DC and New York City using the Routes API. The result is a data frame with one row per origin - destination pair; `duration_base` is the travel time without traffic and `duration_traffic` the time in traffic (the two are equal unless traffic is considered).

``` r
results <- distance_routes(origin = "Washington DC",
                           destination = "New York City NY",
                           mode = "DRIVE",
                           key = Sys.getenv("GOOGLE_API_KEY")) # your actual API key comes here...
results
#          origin      destination  mode status distance_text distance_value duration_base duration_traffic
# 1 Washington DC New York City NY DRIVE     OK        362 km         361969         14096            14096
```

Several pairs can be computed in one call, optionally with a departure time (as `POSIXct`, with an explicit time zone) and a traffic model:

``` r
results <- distance_routes(origin = c("Washington DC", "New York NY"),
                           destination = c("Los Angeles CA", "Austin TX"),
                           mode = "DRIVE",
                           departure = as.POSIXct("2026-11-14 23:13:20", tz = "America/New_York"),
                           traffic_model = "PESSIMISTIC",
                           key = Sys.getenv("GOOGLE_API_KEY"))
results[, c("origin", "destination", "distance_value", "duration_base", "duration_traffic")]
#          origin    destination distance_value duration_base duration_traffic
# 1 Washington DC Los Angeles CA        4295051        140529           158915
# 2   New York NY      Austin TX        2803898         93205           103660
```

An example of legacy function `gmapsdistance()` to generate a distance matrix is:

``` r
results <- gmapsdistance(origin = c("Washington DC", "New York NY", "Seattle WA", "Miami FL"),
                         destination = c("Washington DC", "New York NY", "Seattle WA", "Miami FL"),
                         mode = "bicycling",
                         key = Sys.getenv("GOOGLE_API_KEY")) # your actual API key comes here...

results$Distance
```

More examples are in the package vignette.

## Usage limits
The number and size of requests are limited by Google; see the limits of the [Routes API](https://developers.google.com/maps/documentation/routes/usage-and-billing) and of the [Distance Matrix API](https://developers.google.com/maps/documentation/distance-matrix/usage-and-billing).

## License
[GNU General Public License v3.0](https://github.com/jlacko/gmapsdistance/blob/master/LICENSE.md)

## How to contribute
We encourage any kind of suggestions to improve the quality of this code. You can submit pull requests indicating clearly what is the purpose of the change and why we should accept such pull request. Although not necessary, we encourage you to verify that your suggestions are in accordance with the general guidelines established in the CRAN repository by running the R CMD check command.

## Code of conduct
Please see the file [CODE_OF_CONDUCT.md](https://github.com/jlacko/gmapsdistance/blob/master/CODE_OF_CONDUCT.md) for the Code of Conduct for the Contributor Covenant Code of Conduct. 


## Authors
This code was developed originally by [Rodrigo Azuero](http://rodrigoazuero.com/) and [David Zarruk](https://www.davidzarruk.com/).

It is currently maintained by [Jindra Lacko](mailto:jindra.lacko@gmail.com).

[AUTHORS.md](https://github.com/jlacko/gmapsdistance/blob/master/AUTHORS.md) has a list of everyone who has contributed to gmapsdistance.

## Where has gmapsdistance been used or mentioned
We like to keep track of the projects where gmapsdistance has been used. This will help us identify how to better improve the code. Let us know if you use gmapsdistance! Below you will find links to some of the projects and some of the references to gmapsdistance that we have found. 

1. Proximity to pediatric cardiac specialty care for adolescents with congenital heart defects. [Link to article](https://doi.org/10.1002/bdr2.1129). 

2. Measuring Accessibility to Rail Transit Stations in Scarborough: Subway vs. LRT. [Link to article](https://www.torontomu.ca/content/dam/tedrogersschool/documents/Measuring%20accessibility%20to%20rail%20transit%20stations%20in%20Scarborough-final.pdf)

3. Social Data Science Course. University of Copenhagen. Department of Economics. [Link](https://sebastianbarfort.github.io/sds_summer/slides/gathering.pdf)

4. R-bloggers. The collaborative innovation landscape in data science. [Link](https://www.r-bloggers.com/2016/07/the-collaborative-innovation-landscape-in-data-science/)

5. RPubs. [Link](https://rpubs.com/mattdray/gmapsdistance-test) 

6. Identifying Partnership Opportunities at Air Force Installations: A Geographic Information Systems Approach [Link](https://scholar.afit.edu/etd/810/) 

7. DataWookie. Review of gmapsdistance. [Link](https://datawookie.dev/blog/2017/08/route-asymmetry-in-google-maps/)

8. The Role of Telehealth in Promoting Equitable Abortion Access in the United States: Spatial Analysis [Link to article](https://doi.org/10.2196/45671)

9. The relationship between living in urban and rural areas of Scotland and children’s physical activity and sedentary levels: a country-wide cross-sectional analysis [Link to article](https://doi.org/10.1186/s12889-020-8311-y)

10. E-trikes for urban delivery: An empirical mixed-fleet simulation approach to assess city logistics sustainability [Link to article](https://doi.org/10.1016/j.scs.2023.104641)
