

qcs_read_dir_path <- function(qcs_dir_path, dataset_name) {
  output_dir_path <- paste0(qcs_dir_path, "/output/", dataset_name)
  output_dir_path <- normalizePath(output_dir_path, winslash = "/",
                                   mustWork = FALSE)
  return(output_dir_path)
}

#' @title JRC-ENCR QCS Results
#' @description
#' Read JRC-ENCR QCS results into R.
#' @template param_qcs_dir_path
#' @eval c(
#'   arg_dataset_name_docs(),
#'   codedoc::codedoc_lines("encrqcs::qcs_read_results::", "R/read.R"),
#'   "@details",
#'   codedoc::codedoc_lines("^\\Qdetails(encrqcs::qcs_read_results)\\E$", "R/read.R"),
#'   "@return",
#'   codedoc::codedoc_lines("^\\Qreturn(encrqcs::qcs_read_results)\\E$", "R/read.R")
#' )
#' @template param_assertion_type
#' @export
qcs_read_results <- function(
    qcs_dir_path,
    dataset_name,
    fread_arg_list = NULL,
    readlines_arg_list = NULL,
    assertion_type = "input"
) {
  # assertions -----------------------------------------------------------------
  encrqcs::assert_is_qcs_dataset_name(dataset_name,
                                      assertion_type = assertion_type)
  dbc::assert_dir_exists(qcs_dir_path, assertion_type = assertion_type)
  # @codedoc_comment_block details(encrqcs::qcs_read_results)
  # `[encrqcs::qcs_read_results]` performs the following steps:
  #
  # 1. Directory containing results is assumed to look like e.g.
  #    `${encrqcs:::qcs_read_dir_path("C:/path/to/qcs/", "incidence")}` for
  #    `qcs_dir_path = "C:/path/to/qcs/"`, `dataset_name = "incidence"`.
  # @codedoc_comment_block details(encrqcs::qcs_read_results)
  output_dir_path <- qcs_read_dir_path(qcs_dir_path, dataset_name)
  if (!dir.exists(output_dir_path)) {
    stop("No such directory: ", deparse(output_dir_path), ". Expected that ",
         "directory to contain output for dataset_name = ",
         deparse(dataset_name), " results.")
  }

  # output_file_paths ----------------------------------------------------------
  # @codedoc_comment_block details(encrqcs::qcs_read_results)
  # 2. Result files in the results dir are those that match case-insensitive
  #    regex `"output([.]txt)|([.]csv)"`.
  # @codedoc_comment_block details(encrqcs::qcs_read_results)
  output_file_names <- dir(output_dir_path, pattern = "output([.]txt)|([.]csv)",
                           ignore.case = TRUE)
  output_file_paths <- dir(output_dir_path, pattern = "output([.]txt)|([.]csv)",
                           full.names = TRUE, ignore.case = TRUE)
  if (length(output_file_names) == 0L) {
    stop("No .csv nor .txt file in output dir ", deparse(output_dir_path),
         ". Did the previous JRC-ENCR QCS run finish successfully?")
  }

  # @codedoc_comment_block return(encrqcs::qcs_read_results)
  #    The output will be a list with one
  #    element for each result file. The name of the element is the name of
  #    the file in the results dir.
  # @codedoc_comment_block return(encrqcs::qcs_read_results)
  names(output_file_paths) <- output_file_names
  lapply(output_file_paths, function(file_path) {
    # @codedoc_comment_block details(encrqcs::qcs_read_results)
    # 3. Each results file is read separately into R. Those ending in
    #    `.txt` are read using `[readLines]`. Those ending in `.csv` are read
    #    using `[data.table::fread]`.
    # @codedoc_insert_comment_block return(encrqcs::qcs_read_results)
    # @codedoc_comment_block details(encrqcs::qcs_read_results)
    if (grepl("txt$", file_path)) {
      # @codedoc_comment_block encrqcs::qcs_read_results::readlines_arg_list
      # @param readlines_arg_list `[NULL, list]` (default `NULL`)
      #
      # Additional arguments passed to `[readLines]` if a `list`.
      # Argument `con` is determined internally and cannot be changed.
      # @codedoc_comment_block encrqcs::qcs_read_results::readlines_arg_list
      readlines_arg_list <- as.list(readlines_arg_list)
      readlines_arg_list[["con"]] <- file_path
      do.call(readLines, readlines_arg_list, quote = TRUE)
    } else {
      # @codedoc_comment_block encrqcs::qcs_read_results::fread_arg_list
      # @param fread_arg_list `[NULL, list]` (default `NULL`)
      #
      # Additional arguments passed to `[data.table::fread]` if a `list`.
      # Argument `file` is determined internally and cannot be changed.
      # @codedoc_comment_block encrqcs::qcs_read_results::fread_arg_list
      fread_arg_list <- as.list(fread_arg_list)
      fread_arg_list[["file"]] <- file_path
      do.call(data.table::fread, fread_arg_list, quote = TRUE)
    }
  })
}

