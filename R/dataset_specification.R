




#' @title JRC-ENCR QCS Dataset
#' @description
#' Collect a QCS dataset. Optional columns can be omitted and will be populated
#' with `NA` values.
#' @param dataset `[data.frame]` (mandatory, no default)
#'
#' dataset to form into the standard QCS format
#' @template param_dataset_name
#' @export
#' @importFrom dbc assert_is_data.frame_with_required_names
qcs_dataset <- function(
  dataset,
  dataset_name
) {
  assert_is_qcs_dataset_name(dataset_name)

  core_col_nms <- qcs_column_names(dataset_name = dataset_name,
                                   core = TRUE)
  noncore_col_nms <- qcs_column_names(dataset_name = dataset_name,
                                      core = FALSE)
  all_col_nms <- qcs_column_names(dataset_name = dataset_name,
                                  core = NA)
  dbc::assert_is_data.frame_with_required_names(
    x = dataset,
    x_nm = "dataset",
    required_names = core_col_nms
  )

  dt <- data.table::copy(data.table::setDT(
    mget(
      all_col_nms,
      envir = as.environment(dataset),
      ifnotfound = list(rep(NA_character_, nrow(dataset)))
    )
  ))

  return(dt[])
}


