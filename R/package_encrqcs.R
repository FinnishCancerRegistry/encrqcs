
#' encrqcs
#'
#' @name encrqcs
#' @docType package
#' @title `encrqcs`: JRC-ENCR QCS Tools
#' @eval c(
#'   codedoc::codedoc_R_package_description("encrqcs")[-(2:9)],
#'   codedoc::codedoc_news_for_R_package()
#' )
NULL

# @codedoc_comment_block R_package_description(encrqcs)
#
# <!-- badges: start -->
# [![R-CMD-check](https://github.com/finnishcancerregistry/encrqcs/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/finnishcancerregistry/encrqcs/actions/workflows/R-CMD-check.yaml)
# <!-- badges: end -->
#
# Use JRC-ENCR QCS (https://encr.eu/tools-for-registries) from the
# comfort of your R session.
#
# ## Installation
#
# You can install the development version of encrqcs from
# [GitHub](https://github.com/) with:
#
#   ``` r
# # install.packages("devtools")
# devtools::install_github("finnishcancerregistry/encrqcs")
# ```
#
# ## Example
#
# Run incidence checks using JRC-ENCR QCS:
#
# ``` r
# results <- encrqcs::qcs_run(
#   my_incidence, "incidence", "path/to/qcs/root/dir/"
# )
# ```
#
# @codedoc_comment_block R_package_description(encrqcs)

