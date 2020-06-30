




#' @title JRC-ENCR QCS Input and Output
#' @description
#' Read and write .csv files for / created by the JRC-ENCR QCS.
#' @template param_dataset
#' @template param_dataset_name
#' @param file_path `[character]` (mandatory, no default)
#'
#' path to write file to
#' @param ...
#'
#' additional arguments passed to [data.table::fwrite]
#' @export
write_qcs_file <- function(
  dataset,
  dataset_name = "incidence",
  file_path,
  ...
) {
  assert_is_qcs_dataset_name(dataset_name)
  assert_is_qcs_dataset(dataset, dataset_name)

  data.table::fwrite(
    x = dataset,
    file = file_path,
    sep = ";",
    ...
  )
}






