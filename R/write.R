




#' @title JRC-ENCR QCS Input and Output
#' @description
#' Read and write .csv files for / created by the JRC-ENCR QCS.
#' @template param_dataset
#' @eval c(
#'   arg_dataset_name_docs(),
#'   codedoc::codedoc_lines("^encrqcs::qcs_write_dataset::", "R/write.R"),
#'   "@details",
#'   codedoc::codedoc_lines("^\\Qdetails(encrqcs::qcs_write_dataset)\\E$", "R/write.R"),
#'   "@return",
#'   codedoc::codedoc_lines("^\\Qreturn(encrqcs::qcs_write_dataset)\\E$", "R/write.R")
#' )
#' @param file_path `[character]` (no default)
#'
#' path to write file to
#' @template param_assertion_type
#' @export
qcs_write_dataset <- function(
  dataset,
  dataset_name,
  file_path,
  fwrite_arg_list = NULL,
  assertion_type = NULL
) {
  # assertions -----------------------------------------------------------------
  encrqcs::assert_is_qcs_dataset_name(
    dataset_name,
    assertion_type = assertion_type
  )
  encrqcs::assert_is_qcs_dataset(
    dataset, assertion_type = assertion_type,
    dataset_name = dataset_name
  )
  dbc::assert_is_one_of(
    fwrite_arg_list,
    funs = list(dbc::report_is_NULL, dbc::report_is_list)
  )

  # fwrite ---------------------------------------------------------------------
  # @codedoc_comment_block encrqcs::qcs_write_dataset::fwrite_arg_list
  # @param fwrite_arg_list `[NULL, list]` (default `NULL`)
  #
  # Additional arguments passed to `[data.table::fwrite]` if a list. Arguments
  # `x`, `file`, and `sep` are determined internally and cannot be changed.
  # @codedoc_comment_block encrqcs::qcs_write_dataset::fwrite_arg_list
  fwrite_arg_list <- as.list(fwrite_arg_list)
  fwrite_arg_list[c("x", "file", "sep")] <- list(
    x = dataset, file = file_path, sep = ";"
  )
  # @codedoc_comment_block details(encrqcs::qcs_write_dataset)
  # `[encrqcs::qcs_write_dataset]` performs simple checks on its input and
  # writes `dataset` on the disk using `[data.table::fwrite]`.
  # @codedoc_comment_block details(encrqcs::qcs_write_dataset)
  do.call(data.table::fwrite, fwrite_arg_list, quote = TRUE)

  # @codedoc_comment_block return(encrqcs::qcs_write_dataset)
  # `[encrqcs::qcs_write_dataset]` always returns `NULL` invisibly. As a
  # side effect it writes `dataset` to disk.
  # @codedoc_comment_block return(encrqcs::qcs_write_dataset)
  return(NULL)
}
