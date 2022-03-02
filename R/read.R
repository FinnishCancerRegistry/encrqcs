

qcs_read_dir_path <- function(qcs_dir_path, dataset_name) {
  output_dir_path <- paste0(qcs_dir_path, "/output/", dataset_name)
  output_dir_path <- normalizePath(output_dir_path, winslash = "/",
                                   mustWork = FALSE)
  return(output_dir_path)
}

#' @title JRC-ENCR Results
#' @description
#' Read JRC-ENCR results into R.
#' @template param_qcs_dir_path
#' @eval arg_dataset_name_docs()
#' @param fread_arg_list `[NULL, list]` (default `NULL`)
#'
#' Additional arguments passed to `[data.table::fread]` if a `list`.
#' @param readlines_arg_list `[NULL, list]` (default `NULL`)
#'
#' Additional arguments passed to `[readLines]` if a `list`.
#' @template param_assertion_type
#' @export
qcs_read_results <- function(
    qcs_dir_path,
    dataset_name,
    fread_arg_list = NULL,
    readlines_arg_list = NULL,
    assertion_type = "input"
) {
  encrqcs::assert_is_qcs_dataset_name(dataset_name,
                                      assertion_type = assertion_type)
  dbc::assert_dir_exists(qcs_dir_path)
  output_dir_path <- qcs_read_dir_path(qcs_dir_path = qcs_dir_path,
                                       dataset_name = dataset_name)
  if (!dir.exists(output_dir_path)) {
    stop("No such directory: ", deparse(output_dir_path), ". Expected that ",
         "directory to contain output for dataset_name = ",
         deparse(dataset_name), " results.")
  }
  output_file_names <- dir(output_dir_path, pattern = "([.]txt)|([.]csv)")
  output_file_paths <- dir(output_dir_path, pattern = "([.]txt)|([.]csv)",
                           full.names = TRUE)
  if (length(output_file_names) == 0L) {
    stop("No .csv nor .txt file in output dir ", deparse(output_dir_path),
         ". Did the previous JRC-ENCR QCS run finish successfully?")
  }
  names(output_file_paths) <- output_file_names
  lapply(output_file_paths, function(file_path) {
    if (grepl("txt$", file_path)) {
      readlines_arg_list <- as.list(readlines_arg_list)
      readlines_arg_list[["con"]] <- file_path
      do.call(readLines, readlines_arg_list, quote = TRUE)
    } else {
      fread_arg_list <- as.list(fread_arg_list)
      fread_arg_list[["file"]] <- file_path
      do.call(data.table::fread, fread_arg_list, quote = TRUE)
    }
  })
}

