
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![crputils status
badge](https://pifsc-protected-species-division.r-universe.dev/badges/crputils)](https://pifsc-protected-species-division.r-universe.dev/crputils)

# crputils

<!-- badges: start -->
<!-- badges: end -->

Miscellaneous R utilities used by CRP

What?! It’s an R package!!

## Installation

### R-Universe

#### Binary

``` r
install.packages('crputils', 
                 repos=c('https://pifsc-protected-species-division.r-universe.dev','https://cloud.r-project.org')
)
```

#### Source

*You will need a C++ compiler for R*

``` r
install.packages('crputils', type='source', 
                 repos=c('https://pifsc-protected-species-division.r-universe.dev','https://cloud.r-project.org')
)
```

### GitHub

*You will need a C++ compiler for R*

``` r
remotes::install_github('pifsc-protected-species-division/crputils')
```

## Components:

### Latitude/longitude format conversions

Functions to convert between different latitude/longitude formats.
Includes:

- Decimal degrees (DD) TO degrees decimal seconds (DMM) or degrees
  minutes seconds (DMS)
- Degrees decimal minutes (DMM) TO decimal degrees (DD) or degrees
  minutes seconds (DMS)
- Degrees minutes seconds (DMS) TO decimal degrees (DD) or degrees
  decimal minutes (DMM)

See
[`exampleWorkflows/workflow_latLonCov`](https://github.com/PIFSC-Protected-Species-Division/crputils/blob/main/exampleWorkflows/workflow_latLonConv.R)
for example script to use these functions.

### GPX creation

Functions to convert tables of lat/lon data to GPX files readable by
Coastal Explorer

## Disclaimer

*This software package is developed and maintained by scientists at the
NOAA Fisheries Pacific Islands Fisheries Science Center and should be
considered a fundamental research communication. The recommendations and
conclusions presented here are those of the authors and this software
should not be construed as official communication by NMFS, NOAA, or the
U.S. Dept. of Commerce. In addition, reference to trade names does not
imply endorsement by the National Marine Fisheries Service, NOAA. While
the best efforts have been made to insure the highest quality, tools
such as this are under constant development and are subject to change.*
