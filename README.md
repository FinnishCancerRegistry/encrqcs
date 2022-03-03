
# encrqcs

<!-- badges: start -->
[![R-CMD-check](https://github.com/finnishcancerregistry/encrqcs/workflows/R-CMD-check/badge.svg)](https://github.com/finnishcancerregistry/encrqcs/actions)
<!-- badges: end -->


`encrqcs`: Use JRC-ENCR QCS (https://encr.eu/tools-for-registries) from the 
comfort of your R session.

## Installation

You can install the development version of encrqcs from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("finnishcancerregistry/encrqcs")
```

## Example

Run incidence checks using JRC-ENCR QCS:

``` r
results <- encrqcs::qcs(my_incidence, "incidence", "path/to/qcs/root/dir/")
```

Run mortality checks using JRC-ENCR QCS:

``` r
results <- encrqcs::qcs(my_mortality, "mortality", "path/to/qcs/root/dir/")
```