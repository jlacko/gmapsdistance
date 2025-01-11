## version 4.0.5 (2025-01-11)

- update of tests and readme, without impact on core functionality

## version 4.0.4 (2023-08-10)

- removed dead links from README to pass CRAN incoming checks

## version 4.0.3 (2023-08-04)

- corrected errors introduced by scientific formatting of seconds since Unix Epoch
- moved dates in vignette to the future (only future dates are considered legal by the Google API)

## version 4.0.2 (2023-05-26)

- consider arrival date & time when creating url for API call when using public transport / #5

## version 4.0.1 (2022-08-30)

- push the dates in vignette back to the future (the code won't run in the past)
- update of unit tests

## version 4.0.0 (2022-04-26)
 
 - [!] change of package maintainer / from Rodrigo Azuelo to Jindra Lacko
 - [!] API call optimized to prioritize Distance Matrix over Distance Matrix Advanced calls
 - improved handling of non-ASCII place names (and spaces)
 - format of "wide" output changed to matrix
 - introduced unit tests
 - internal optimization + bugfixes
