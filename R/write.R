




#' @title JRC-ENCR QCS Input and Output
#' @description
#' Read and write .csv files for / created by the JRC-ENCR QCS.
#' @template param_dataset
#' @eval arg_dataset_name_docs()
#' @param file_path `[character]` (no default)
#'
#' path to write file to
#' @param fwrite_arg_list `[NULL, list]` (default `NULL`)
#'
#' additional arguments passed to [data.table::fwrite]
#' @template param_assertion_type
#' @export
qcs_write_dataset <- function(
  dataset,
  dataset_name,
  file_path,
  fwrite_arg_list = NULL,
  assertion_type = "input"
) {
  assert_is_qcs_dataset_name(dataset_name, assertion_type = assertion_type)
  assert_is_qcs_dataset(dataset, assertion_type = assertion_type,
                        dataset_name = dataset_name)
  dbc::assert_is_one_of(
    fwrite_arg_list,
    funs = list(dbc::report_is_NULL, dbc::report_is_list)
  )

  fwrite_arg_list <- as.list(fwrite_arg_list)
  fwrite_arg_list[c("x", "file", "sep")] <- list(
    x = dataset, file = file_path, sep = ";"
  )
  do.call(data.table::fwrite, fwrite_arg_list, quote = TRUE)
}






