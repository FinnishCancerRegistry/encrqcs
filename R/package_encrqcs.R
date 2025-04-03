
#' encrqcs
#'
#' @name encrqcs
#' @title `encrqcs`: JRC-ENCR QCS Tools
#' @eval c(
#'   codedoc::codedoc_R_package_description("encrqcs")[-(2:9)],
#'   codedoc::codedoc_news_for_R_package()
#' )
"_PACKAGE"

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
# You can install the release of encrqcs from
# [GitHub](https://github.com/FinnishCancerRegistry/encrqcs) with:
#
#   ``` r
# # install.packages("devtools")
# devtools::install_github("finnishcancerregistry/encrqcs@release")
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

# @codedoc_comment_block news("encrqcs", "2025-02-11", "0.3.0")
# Bump dependencies.
# @codedoc_comment_block news("encrqcs", "2025-02-11", "0.3.0")
