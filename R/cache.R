#' @title `encrqcs` Cache
#' @description
#' Functions to make use of the cache used by `encrqcs`.
#' @name qcs_cache
NULL

#' @eval codedoc::pkg_doc_fun(
#'   "encrqcs::qcs_cache_dir_path",
#'   "qcs_cache"
#' )
qcs_cache_dir_path <- function(
  qcs_dir_path
) {
  # @codedoc_comment_block news("encrqcs::qcs_cache_dir_path", "2025-06-10", "1.0.0")
  # New function `encrqcs::qcs_cache_dir_path`.
  # @codedoc_comment_block news("encrqcs::qcs_cache_dir_path", "2025-06-10", "1.0.0")
  #' @template param_qcs_dir_path
  dbc::assert_dir_exists(qcs_dir_path)
  dbc::assert_has_length(qcs_dir_path, expected_length = 1L)
  qcs_dir_path <- normalizePath(path = qcs_dir_path, winslash = "/")

  # @codedoc_comment_block encrqcs::qcs_cache_dir_path
  # `encrqcs::qcs_cache_dir_path` returns the path to the cache dir used by
  # `encrqcs` given `qcs_dir_path` where the JRC-ENCR QCS is located.
  # It is simply `sprintf("%s/temp/encrqcs/", qcs_dir_path)`.
  # @codedoc_comment_block encrqcs::qcs_cache_dir_path
  qcs_cache_dir_path <- sprintf("%s/temp/encrqcs/", qcs_dir_path)
  if (!dir.exists(qcs_cache_dir_path)) {
    dir.create(qcs_cache_dir_path, recursive = TRUE)
  }
  return(qcs_cache_dir_path)
}

#' @eval codedoc::pkg_doc_fun(
#'   "encrqcs::qcs_cache_metadata_file_path",
#'   "qcs_cache"
#' )
qcs_cache_metadata_file_path <- function(
  qcs_dir_path
) {
  # @codedoc_comment_block news("encrqcs::qcs_cache_metadata_file_path", "2025-06-10", "1.0.0")
  # New function `encrqcs::qcs_cache_metadata_file_path`.
  # @codedoc_comment_block news("encrqcs::qcs_cache_metadata_file_path", "2025-06-10", "1.0.0")
  dbc::assert_dir_exists(qcs_dir_path)
  dbc::assert_has_length(qcs_dir_path, expected_length = 1L)
  qcs_dir_path <- normalizePath(path = qcs_dir_path, winslash = "/")

  # @codedoc_comment_block encrqcs::qcs_cache_metadata_file_path
  # `encrqcs::qcs_cache_metadata_file_path` returns the path to metadata file
  # in the cache.
  # It is simply `sprintf("%s/metadata.csv", qcs_cache_dir_path(qcs_dir_path))`.
  # @codedoc_comment_block encrqcs::qcs_cache_metadata_file_path
  qcs_cache_metadata_file_path <- sprintf(
    "%s/metadata.csv",
    encrqcs::qcs_cache_dir_path(qcs_dir_path)
  )
  qcs_cache_metadata_file_path <- normalizePath(
    qcs_cache_metadata_file_path,
    mustWork = FALSE,
    winslash = "/"
  )
  return(qcs_cache_metadata_file_path)
}

#' @eval codedoc::pkg_doc_fun(
#'   "encrqcs::qcs_cache_metadata_read",
#'   "qcs_cache"
#' )
qcs_cache_metadata_read <- function(
  qcs_dir_path
) {
  # @codedoc_comment_block news("encrqcs::qcs_cache_metadata_read", "2025-06-10", "1.0.0")
  # New function `encrqcs::qcs_cache_metadata_read`.
  # @codedoc_comment_block news("encrqcs::qcs_cache_metadata_read", "2025-06-10", "1.0.0")
  # @codedoc_comment_block encrqcs::qcs_cache_metadata_read
  # `encrqcs::qcs_cache_metadata_read` reads (with `[data.table::fread]`)
  # the file at `encrqcs::qcs_cache_metadata_file_path(qcs_dir_path)`. If it
  # does not exist, an empty `data.table` is returned. Currently
  # the metadata file maps `hash` to `run_id`, where the former is computed on
  # a dataset and `run_id` corresponds to column `RUN_ID` in
  # `temp/validation_run.csv`. This maps a dataset to a previous run reliably.
  # @codedoc_comment_block encrqcs::qcs_cache_metadata_read
  path <- encrqcs::qcs_cache_metadata_file_path(qcs_dir_path)
  if (!file.exists(path)) {
    return(data.table::data.table(
      metadata_datetime = Sys.time()[0],
      hash = character(0L),
      qcs_protocol_id = integer(0L),
      run_id = integer(0L)
    )[])
  } else {
    return(data.table::fread(path, encoding = "UTF-8")[])
  }
}

#' @eval codedoc::pkg_doc_fun(
#'   "encrqcs::qcs_cache_metadata_write",
#'   "qcs_cache"
#' )
qcs_cache_metadata_write <- function(
  cache_metadata_dt,
  qcs_dir_path
) {
  # @codedoc_comment_block news("encrqcs::qcs_cache_metadata_write", "2025-06-10", "1.0.0")
  # New function `encrqcs::qcs_cache_metadata_write`.
  # @codedoc_comment_block news("encrqcs::qcs_cache_metadata_write", "2025-06-10", "1.0.0")
  # @codedoc_comment_block encrqcs::qcs_cache_metadata_write
  # `encrqcs::qcs_cache_metadata_write` writes `cache_metadata_dt` with
  # `[data.table::fwrite]` into
  # `encrqcs::qcs_cache_metadata_file_path(qcs_dir_path)`.
  # @codedoc_comment_block encrqcs::qcs_cache_metadata_write
  #' @param cache_metadata_dt `[data.table]` (no default)
  #'
  #' Table of metadata to write into cache. Must have these columns:
  #' `c("metadata_datetime", "hash", "qcs_protocol_id", "run_id")`.
  dbc::assert_is_data_frame_with_required_names(
    x = cache_metadata_dt,
    required_names = c("metadata_datetime", "hash", "qcs_protocol_id", "run_id")
  )
  path <- encrqcs::qcs_cache_metadata_file_path(qcs_dir_path)
  data.table::fwrite(
    x = cache_metadata_dt,
    file = path,
    encoding = "UTF-8"
  )
  return(invisible(NULL))
}

qcs_cache_validation_run_file_path__ <- function(qcs_dir_path) {
  return(sprintf("%s/temp/validation_run.csv", qcs_dir_path))
}
qcs_cache_validation_run_file_read__ <- function(qcs_dir_path) {
  vr_file_path <- qcs_cache_validation_run_file_path__(qcs_dir_path)
  if (!file.exists(vr_file_path)) {
    vr_dt <- data.table::data.table(RUN_ID = integer(0L))
  } else {
    vr_dt <- data.table::fread(file = vr_file_path)
  }
  return(vr_dt[])
}

#' @eval codedoc::pkg_doc_fun(
#'   "encrqcs::qcs_cache_metadata_update",
#'   "qcs_cache"
#' )
#' @template param_qcs_dir_path
#' @template param_qcs_protocol_id
qcs_cache_metadata_update <- function(
  qcs_dir_path,
  qcs_protocol_id,
  run_id,
  hash = NULL,
  dataset_file_path = NULL
) {
  # @codedoc_comment_block news("encrqcs::qcs_cache_metadata_update", "2025-06-10", "1.0.0")
  # New function `encrqcs::qcs_cache_metadata_update`.
  # @codedoc_comment_block news("encrqcs::qcs_cache_metadata_update", "2025-06-10", "1.0.0")
  # @codedoc_comment_block encrqcs::qcs_cache_metadata_update
  # `encrqcs::qcs_cache_metadata_update` the metadata file with
  # `encrqcs::qcs_cache_metadata_read`, adds a new record into it,
  # and re-writes the metadata with `encrqcs::qcs_cache_metadata_write`.
  # @codedoc_comment_block encrqcs::qcs_cache_metadata_update
  cache_metadata_dt <- encrqcs::qcs_cache_metadata_read(qcs_dir_path)
  vr_dt <- qcs_cache_validation_run_file_read__(qcs_dir_path = qcs_dir_path)
  #' @param run_id `[integer]` (no default)
  #'
  #' One of the values of column `RUN_ID` in `temp/validation_run.csv` under
  #' the `qcs_dir_path`.
  dbc::assert_is_integer_nonNA_gtezero_atom(run_id)
  cache_metadata_dt <- cache_metadata_dt[
    cache_metadata_dt[["run_id"]] %in% vr_dt[["RUN_ID"]]
  ]
  #' @template param_optional_hash
  if (is.null(hash)) {
    #' @template param_optional_dataset_file_path
    hash <- qcs_cache_dataset_file_hash(dataset_file_path)
  }
  cache_metadata_dt <- rbind(
    data.table::data.table(
      metadata_datetime = Sys.time(),
      hash = hash,
      qcs_protocol_id = qcs_protocol_id,
      run_id = run_id
    )
  )
  encrqcs::qcs_cache_metadata_write(
    cache_metadata_dt = cache_metadata_dt,
    qcs_dir_path = qcs_dir_path
  )
}


#' @eval codedoc::pkg_doc_fun(
#'   "encrqcs::qcs_cache_dataset_file_hash",
#'   "qcs_cache"
#' )
qcs_cache_dataset_file_hash <- function(dataset_file_path) {
  # @codedoc_comment_block news("encrqcs::qcs_cache_dataset_file_hash", "2025-06-10", "1.0.0")
  # New function `encrqcs::qcs_cache_dataset_file_hash`.
  # @codedoc_comment_block news("encrqcs::qcs_cache_dataset_file_hash", "2025-06-10", "1.0.0")
  # @codedoc_comment_block encrqcs::qcs_cache_dataset_file_hash
  # `encrqcs::qcs_cache_dataset_file_hash` computes the hash using
  # `[digest::digest]` on an on-disk dataset. It uses
  # `algo = "md5"`.
  # @codedoc_comment_block encrqcs::qcs_cache_dataset_file_hash
  return(digest::digest(
    file = dataset_file_path,
    algo = "md5"
  ))
}

qcs_cache_metadata_for_dataset__ <- function(
  qcs_dir_path,
  hash
) {
  dbc::assert_is_character_nonNA_atom(hash)
  md_dt <- qcs_cache_metadata_read(qcs_dir_path)
  idx <- data.table::chmatch(hash, md_dt[["hash"]])
  if (is.na(idx)) {
    return(list())
  }
  out <- as.list(md_dt[idx, ])
  vr_dt <- qcs_cache_validation_run_file_read__(qcs_dir_path = qcs_dir_path)
  vr_dt_idx <- match(out[["run_id"]], vr_dt[["RUN_ID"]])
  out[["run_data"]] <- as.list(vr_dt[vr_dt_idx, ])
  return(out)
}

qcs_cache_has_results_for_dataset__ <- function(
  qcs_dir_path,
  hash
) {
  return(length(qcs_cache_metadata_for_dataset__(qcs_dir_path, hash)) > 0)
}
