qcs_read_dir_path <- function(
  qcs_dir_path,
  qcs_protocol_id,
  type
) {
  output_dir_path <- switch(
    type,
    summary = {
      # @codedoc_comment_block details(encrqcs::qcs_read_results)
      # `[encrqcs::qcs_read_results]` performs the following steps:
      #
      # - Directory containing summary results is assumed to look like e.g.
      #   `${encrqcs:::qcs_read_dir_path("C:/path/to/qcs/", "incidence", "summary")}`
      #   for `qcs_dir_path = "C:/path/to/qcs/"`, `qcs_protocol_id = "incidence"`.
      dataset_name <- handle_qcs_protocol_id(
        qcs_protocol_id, output_type = "dataset_name"
      )
      paste0(qcs_dir_path, "/output/", dataset_name)
    },
    # @codedoc_comment_block details(encrqcs::qcs_read_results)
    # - Directory containing the record-level results is assumed to look like e.g.
    #   `${encrqcs:::qcs_read_dir_path("C:/path/to/qcs/", "incidence", "record")}`
    #   .
    # @codedoc_comment_block details(encrqcs::qcs_read_results)
    record = paste0(qcs_dir_path, "/temp/"),
    stop("Internal error: unknown type = ", deparse1(type))
  )
  output_dir_path <- normalizePath(output_dir_path, winslash = "/",
                                   mustWork = FALSE)
  if (!dir.exists(output_dir_path)) {
    stop("Could not read QCS results: directory ", output_dir_path, " does ",
         "not exist. Did the QCS call succeed?")
  }
  return(output_dir_path)
}

qcs_read_file_paths <- function(
  qcs_dir_path,
  qcs_protocol_id,
  type
) {
  if (type != "all") {
    output_dir_path <- qcs_read_dir_path(
      qcs_dir_path = qcs_dir_path,
      qcs_protocol_id = qcs_protocol_id,
      type = type
    )
  }
  out <- switch(
    type,
    # @codedoc_comment_block details(encrqcs::qcs_read_results)
    # - Only the file `QCS-Incidence-Output-Summary.csv` is read from the
    #   summary directory.
    # @codedoc_comment_block details(encrqcs::qcs_read_results)
    summary = sprintf("%s/QCS-Incidence-Output-Summary.txt", output_dir_path),
    record = {
      # @codedoc_comment_block details(encrqcs::qcs_read_results)
      # - From the record directory, every .csv belonging to the latest call
      #   of QCS with the requested `qcs_protocol_id` is read, except
      #   those whose file names match regex `"(correct)|(valid)"`.
      # @codedoc_comment_block details(encrqcs::qcs_read_results)
      meta <- data.table::fread(
        sprintf("%s/validation_run.csv", output_dir_path)
      )
      meta <- subset(meta, meta[["PROTOCOL_ID"]] == qcs_protocol_id)
      meta <- as.list(meta[nrow(meta), ])

      csv_file_paths <- dir(
        output_dir_path,
        pattern = sprintf("_%i[.]csv$", meta[["RUN_ID"]]),
        full.names = TRUE
      )
      csv_file_paths <- csv_file_paths[
        !grepl("(correct)|(valid)", basename(csv_file_paths))
      ]
      csv_file_paths
    },
    all = c(
      qcs_read_file_paths(
        qcs_dir_path = qcs_dir_path,
        qcs_protocol_id = qcs_protocol_id,
        type = "summary"
      ),
      qcs_read_file_paths(
        qcs_dir_path = qcs_dir_path,
        qcs_protocol_id = qcs_protocol_id,
        type = "record"
      )
    ),
    stop("Internal error: unknown type = ", deparse1(type))
  )
  out <- normalizePath(
    out,
    winslash = "/",
    mustWork = FALSE
  )
  return(out)
}

#' @title JRC-ENCR QCS Results
#' @description
#' Read JRC-ENCR QCS results into R.
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
  qcs_protocol_id,
  fread_arg_list = NULL,
  readlines_arg_list = NULL,
  assertion_type = NULL,
  dataset_name = NULL
) {
  # @codedoc_comment_block news("encrqcs::qcs_read_results", "2026-01-28", "0.8.0")
  # `encrqcs::qcs_read_results` revamp: it now reads a different and more usable
  # set of results than before.
  # @codedoc_comment_block news("encrqcs::qcs_read_results", "2026-01-28", "0.8.0")
  # assertions -----------------------------------------------------------------
  if (!is.null(dataset_name)) {
    stop("Argument `dataset_name` has been deprecated. Use argument ",
         "`qcs_protocol_id` instead.")
  }
  #' @template param_qcs_dir_path
  dbc::assert_dir_exists(qcs_dir_path, assertion_type = assertion_type)
  #' @template param_qcs_protocol_id
  qcs_protocol_id <- handle_qcs_protocol_id(qcs_protocol_id)

  # output_file_paths ----------------------------------------------------------
  # @codedoc_comment_block news("encrqcs::qcs_read_results", "2025-10-24", "0.7.0")
  # Fixed `encrqcs::qcs_read_results`: it now also reads e.g.
  # `QCS-Incidence-Output-Summary.txt`. The regex used to detect files to read
  # in the results dir was `"output([.]txt)|([.]csv)"` and is now
  # `"output.*([.]txt)|([.]csv)"` (added `.*`).
  # @codedoc_comment_block news("encrqcs::qcs_read_results", "2025-10-24", "0.7.0")

  output_file_paths <- qcs_read_file_paths(
    qcs_dir_path = qcs_dir_path,
    qcs_protocol_id = qcs_protocol_id,
    type = "all"
  )
  output_file_names <- basename(output_file_paths)
  output_file_names <- gsub("(_[0-9]+)?[.][a-zA-Z0-9]+$", "", output_file_names)
  names(output_file_paths) <- output_file_names
  readlines_arg_list <- as.list(readlines_arg_list)
  fread_arg_list <- as.list(fread_arg_list)
  out <- lapply(output_file_paths, function(file_path) {
    # @codedoc_comment_block details(encrqcs::qcs_read_results)
    # - Each results file is read separately into R. Those ending in
    #   `.txt` are read using `[readLines]`. Those ending in `.csv` are read
    #   using `[data.table::fread]`.
    # @codedoc_insert_comment_block return(encrqcs::qcs_read_results)
    # @codedoc_comment_block details(encrqcs::qcs_read_results)
    if (grepl("txt$", file_path)) {
      # @codedoc_comment_block encrqcs::qcs_read_results::readlines_arg_list
      # @param readlines_arg_list `[NULL, list]` (default `NULL`)
      #
      # Additional arguments passed to `[readLines]` if a `list`.
      # Argument `con` is determined internally and cannot be changed.
      # @codedoc_comment_block encrqcs::qcs_read_results::readlines_arg_list
      readlines_arg_list[["con"]] <- file_path
      do.call(readLines, readlines_arg_list, quote = TRUE)
    } else {
      # @codedoc_comment_block encrqcs::qcs_read_results::fread_arg_list
      # @param fread_arg_list `[NULL, list]` (default `NULL`)
      #
      # Additional arguments passed to `[data.table::fread]` if a `list`.
      # Argument `file` is determined internally and cannot be changed.
      # @codedoc_comment_block encrqcs::qcs_read_results::fread_arg_list
      fread_arg_list[["file"]] <- file_path
      do.call(data.table::fread, fread_arg_list, quote = TRUE)
    }
  })
  # @codedoc_comment_block return(encrqcs::qcs_read_results)
  # - The output is a list with one
  #   element for each result file. The name of the element is the name of
  #   the file in the results dir, stripped of file extension and any run
  #   number, e.g. `qcs_rule_output_1.csv` -> `qcs_rule_output`.
  # @codedoc_comment_block return(encrqcs::qcs_read_results)
  # @codedoc_comment_block details(encrqcs::qcs_read_results)
  # @codedoc_insert_comment_block return(encrqcs::qcs_read_results)
  # @codedoc_comment_block details(encrqcs::qcs_read_results)
  return(out)
}
