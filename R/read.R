qcs_read_dir_path <- function(
  qcs_dir_path,
  qcs_protocol_id,
  type,
  must_exist = TRUE
) {
  output_dir_path <- switch(
    type,
    summary = {
      # @codedoc_comment_block details(encrqcs::qcs_read_results)
      # `[encrqcs::qcs_read_results]` performs the following steps:
      #
      # - Directory containing summary results is assumed to look like e.g.
      #   `${encrqcs:::qcs_read_dir_path("C:/path/to/qcs/", "incidence", "summary", FALSE)}`
      #   for `qcs_dir_path = "C:/path/to/qcs/"`, `qcs_protocol_id = "incidence"`.
      # @codedoc_comment_block details(encrqcs::qcs_read_results)
      dataset_name <- handle_qcs_protocol_id(
        qcs_protocol_id, output_type = "dataset_name"
      )
      paste0(qcs_dir_path, "/output/", dataset_name)
    },
    # @codedoc_comment_block details(encrqcs::qcs_read_results)
    # - Directory containing the record-level results is assumed to look like e.g.
    #   `${encrqcs:::qcs_read_dir_path("C:/path/to/qcs/", "incidence", "record", FALSE)}`
    #   .
    # @codedoc_comment_block details(encrqcs::qcs_read_results)
    record = paste0(qcs_dir_path, "/temp/"),
    stop("Internal error: unknown type = ", deparse1(type))
  )
  output_dir_path <- normalizePath(output_dir_path, winslash = "/",
                                   mustWork = FALSE)
  if (must_exist && !dir.exists(output_dir_path)) {
    stop("Could not read QCS results: directory ", output_dir_path, " does ",
         "not exist. Did the QCS call succeed?")
  }
  return(output_dir_path)
}

qcs_read_record_meta_file_path <- function(qcs_dir_path, qcs_protocol_id) {
  meta_dir_path <- qcs_read_dir_path(
    qcs_dir_path = qcs_dir_path,
    qcs_protocol_id = qcs_protocol_id,
    type = "record",
    must_exist = TRUE
  )
  return(sprintf("%s/validation_run.csv", meta_dir_path))
}

qcs_read_record_meta_read <- function(
  qcs_dir_path,
  qcs_protocol_id,
  hash = NULL,
  output = c("list", "table")[1]
) {
  meta_path <- qcs_read_record_meta_file_path(qcs_dir_path, qcs_protocol_id)
  if (!file.exists(meta_path)) {
    return(data.table::data.table(NULL))
  }
  # dont want to see warning about zero rows
  meta <- suppressWarnings(data.table::fread(meta_path))
  if (output == "table") {
    return(meta)
  }
  if (!is.null(hash)) {
    dataset_meta <- qcs_cache_metadata_for_dataset__(
      qcs_dir_path = qcs_dir_path,
      hash = hash
    )
    meta_subset <- meta[["RUN_ID"]] == dataset_meta[["run_id"]]
    if (sum(meta_subset) != 1L) {
      stop(
        "Internal error: ",
        "Attempted to read cached results for `hash = ", hash, "`, but ",
        "there are no results for that hash. If you see this error, please ",
        "complain to the package maintainer."
      )
    }
  } else {
    meta_subset <- meta[["PROTOCOL_ID"]] == qcs_protocol_id
  }
  meta <- subset(meta, meta_subset)
  meta <- as.list(meta[nrow(meta), ])
  return(meta)
}

qcs_read_file_paths <- function(
  qcs_dir_path,
  qcs_protocol_id,
  type,
  hash = NULL
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
      meta <- qcs_read_record_meta_read(
        qcs_dir_path = qcs_dir_path,
        qcs_protocol_id = qcs_protocol_id,
        hash = hash
      )
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
  hash = NULL,
  dataset_file_path = NULL,
  fread_arg_list = NULL,
  readlines_arg_list = NULL,
  assertion_type = NULL
) {
  # @codedoc_comment_block news("encrqcs::qcs_read_results", "2026-01-28", "0.8.0")
  # `encrqcs::qcs_read_results` revamp: it now reads a different and more usable
  # set of results than before.
  # @codedoc_comment_block news("encrqcs::qcs_read_results", "2026-01-28", "0.8.0")
  # @codedoc_comment_block news("encrqcs::qcs_read_results", "2025-06-10", "1.0.0")
  # `encrqcs::qcs_read_results` argument `dataset_name` removed.
  # @codedoc_comment_block news("encrqcs::qcs_read_results", "2025-06-10", "1.0.0")
  # assertions -----------------------------------------------------------------
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

  # @codedoc_comment_block news("encrqcs::qcs_read_results", "2025-06-10", "1.0.0")
  # `encrqcs::qcs_read_results` gains arguments `hash` and `dataset_file_path`,
  # both `NULL` by default.
  # @codedoc_comment_block news("encrqcs::qcs_read_results", "2025-06-10", "1.0.0")
  # @codedoc_comment_block details(encrqcs::qcs_read_results)
  # - If `is.null(hash)` but `dataset_file_path` was supplied, compute the hash
  #   on it.
  # - If we have a `hash` value at this point, we make use of that to determine
  #   which results are read into R. Otherwise we read the latest results for
  #   the supplied `qcs_protocol_id`.
  # @codedoc_comment_block details(encrqcs::qcs_read_results)
  #' @template param_optional_dataset_file_path
  #' @template param_optional_hash
  if (is.null(hash) && !is.null(dataset_file_path)) {
    hash <- qcs_cache_dataset_file_hash(dataset_file_path)
  }
  output_file_paths <- qcs_read_file_paths(
    qcs_dir_path = qcs_dir_path,
    qcs_protocol_id = qcs_protocol_id,
    type = "all",
    hash = hash
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
