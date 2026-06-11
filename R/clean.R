#' @title Clean Output
#' @description
#' Functions to clean QCS output from mass memory.
#' @name qcs_clean
NULL

#' @eval codedoc::pkg_doc_fun("encrqcs::qcs_clean_output", "qcs_clean")
qcs_clean_output <- function(
  qcs_protocol_id,
  qcs_dir_path
) {
  # @codedoc_comment_block news("encrqcs::qcs_clean_output", "2026-01-28", "0.8.0")
  # New function `encrqcs::qcs_clean_output`.
  # @codedoc_comment_block news("encrqcs::qcs_clean_output", "2026-01-28", "0.8.0")
  #' @template param_qcs_protocol_id
  qcs_protocol_id <- handle_arg_qcs_protocol_id__(qcs_protocol_id)
  #' @template param_qcs_dir_path
  # @codedoc_comment_block encrqcs::qcs_clean_output
  # `encrqcs::qcs_clean_output` performs the following steps:
  #
  # - Read a metadata .csv from the `qcs_dir_path`. This contains information
  #   about every previous run of QCS.
  # @codedoc_comment_block encrqcs::qcs_clean_output
  meta <- qcs_read_record_meta_read(
    qcs_dir_path = qcs_dir_path,
    qcs_protocol_id = qcs_protocol_id,
    output = "table"
  )
  if (nrow(meta) <= 1) {
    qcs_clean_output_all(qcs_dir_path)
    return(invisible(NULL))
  }
  # @codedoc_comment_block encrqcs::qcs_clean_output
  # - Detect the latest run in the metadata with the correct `qcs_protocol_id`.
  # @codedoc_comment_block encrqcs::qcs_clean_output
  is_removable_item <- !(meta[["PROTOCOL_ID"]] == qcs_protocol_id &
    duplicated(meta[["PROTOCOL_ID"]], fromLast = TRUE))
  # @codedoc_comment_block encrqcs::qcs_clean_output
  # - Delete the entire directory containing summary data, e.g.
  #   `C:/path/to/qcs/output/incidence`.
  # @codedoc_comment_block encrqcs::qcs_clean_output
  rm_paths <- c(
    qcs_read_dir_path(
      qcs_dir_path = qcs_dir_path,
      qcs_protocol_id = qcs_protocol_id,
      type = "summary",
      must_exist = FALSE
    ),
    # @codedoc_comment_block encrqcs::qcs_clean_output
    # - Delete every .csv file of the detected latest run in
    #   `C:/path/to/qcs/temp`.
    # @codedoc_comment_block encrqcs::qcs_clean_output
    dir(
      qcs_read_dir_path(
        qcs_dir_path = qcs_dir_path,
        qcs_protocol_id = qcs_protocol_id,
        type = "record",
        must_exist = FALSE
      ),
      pattern = sprintf("_%i[.]csv$", meta[["RUN_ID"]][is_removable_item]),
      full.names = TRUE
    )
  )
  # @codedoc_comment_block encrqcs::qcs_clean_output
  # - Remove the latest run from the metadata and write it back, except if
  #   if it was the only run. Then delete that file as well.
  # @codedoc_comment_block encrqcs::qcs_clean_output
  if (nrow(meta) == 1) {
    unlink(qcs_read_record_meta_file_path(
      qcs_dir_path = qcs_dir_path,
      qcs_protocol_id = qcs_protocol_id
    ))
  } else {
    data.table::fwrite(
      subset(
        meta,
        !is_removable_item
      ),
      qcs_read_record_meta_file_path(
        qcs_dir_path = qcs_dir_path,
        qcs_protocol_id = qcs_protocol_id
      ),
      sep = ";",
      encoding = "UTF-8"
    )
  }
  unlink(
    x = rm_paths,
    force = TRUE,
    recursive = TRUE
  )
  return(invisible(NULL))
}

#' @eval codedoc::pkg_doc_fun("encrqcs::qcs_clean_output_all", "qcs_clean")
qcs_clean_output_all <- function(qcs_dir_path) {
  # @codedoc_comment_block news("encrqcs::qcs_clean_output_all", "2026-01-28", "0.8.0")
  # New function `encrqcs::qcs_clean_output_all`.
  # @codedoc_comment_block news("encrqcs::qcs_clean_output_all", "2026-01-28", "0.8.0")
  # @codedoc_comment_block encrqcs::qcs_clean_output_all
  # `encrqcs::qcs_clean_output_all` deletes the subdirectories
  # `output` and `temp` under `qcs_dir_path`.
  # @codedoc_comment_block encrqcs::qcs_clean_output_all
  unlink(
    paste0(qcs_dir_path, "/", c("output", "temp")),
    recursive = TRUE,
    force = TRUE
  )
}
