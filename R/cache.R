#' @title `encrqcs` Cache
#' @description
#' Functions to make use of the cache used by `encrqcs`.
#' @name qcs_cache
NULL

normalize_qcs_dir_path__ <- function(qcs_dir_path) {
  dbc::assert_dir_exists(qcs_dir_path)
  dbc::assert_has_length(qcs_dir_path, expected_length = 1L)
  return(normalizePath(path = qcs_dir_path, winslash = "/"))
}

qcs_cache_metadata_empty__ <- function() {
  data.table::data.table(
    metadata_datetime = Sys.time()[0],
    qcs_dir_path = character(0L),
    hash = character(0L),
    qcs_protocol_id = integer(0L),
    run_id = integer(0L)
  )[]
}

#' @eval codedoc::pkg_doc_fun(
#'   "encrqcs::qcs_cache_dir_path",
#'   "qcs_cache"
#' )
qcs_cache_dir_path <- function() {
  # @codedoc_comment_block news("encrqcs::qcs_cache_dir_path", "2025-06-10", "1.0.0")
  # New function `encrqcs::qcs_cache_dir_path`.
  # @codedoc_comment_block news("encrqcs::qcs_cache_dir_path", "2025-06-10", "1.0.0")

  # @codedoc_comment_block news("encrqcs::qcs_cache_dir_path", "2026-10-15", "1.1.0")
  # `encrqcs::qcs_cache_dir_path` now points to dir `encrqcs` at the root of
  # the QCS dir instead of `temp/encrqcs/` because `temp` is being cleaned
  # by QCS itself.
  # @codedoc_comment_block news("encrqcs::qcs_cache_dir_path", "2026-10-15", "1.1.0")
  # @codedoc_comment_block news("encrqcs::qcs_cache_dir_path", "2026-11-20", "1.2.0")
  # `encrqcs::qcs_cache_dir_path` now points to
  # `tools::R_user_dir("encrqcs", "cache")` instead of a directory under
  # `qcs_dir_path`, and therefore no longer has argument `qcs_dir_path`.
  # The cache is thus shared by all QCS directories; each record in the
  # cache metadata now stores the `qcs_dir_path` it belongs to.
  # Metadata cached by earlier versions under `qcs_dir_path` is not used.
  # @codedoc_comment_block news("encrqcs::qcs_cache_dir_path", "2026-11-20", "1.2.0")
  # @codedoc_comment_block encrqcs::qcs_cache_dir_path
  # `encrqcs::qcs_cache_dir_path` returns the path to the cache dir used by
  # `encrqcs`. It is simply `tools::R_user_dir("encrqcs", "cache")`.
  # The directory is created if it does not exist.
  # @codedoc_comment_block encrqcs::qcs_cache_dir_path
  qcs_cache_dir_path <- tools::R_user_dir("encrqcs", which = "cache")
  if (!dir.exists(qcs_cache_dir_path)) {
    dir.create(qcs_cache_dir_path, recursive = TRUE)
  }
  qcs_cache_dir_path <- normalizePath(qcs_cache_dir_path, winslash = "/")
  return(qcs_cache_dir_path)
}

#' @eval codedoc::pkg_doc_fun(
#'   "encrqcs::qcs_cache_metadata_file_path",
#'   "qcs_cache"
#' )
qcs_cache_metadata_file_path <- function() {
  # @codedoc_comment_block news("encrqcs::qcs_cache_metadata_file_path", "2025-06-10", "1.0.0")
  # New function `encrqcs::qcs_cache_metadata_file_path`.
  # @codedoc_comment_block news("encrqcs::qcs_cache_metadata_file_path", "2025-06-10", "1.0.0")
  # @codedoc_comment_block news("encrqcs::qcs_cache_metadata_file_path", "2026-11-20", "1.2.0")
  # `encrqcs::qcs_cache_metadata_file_path` no longer has argument
  # `qcs_dir_path` because the cache now lives in
  # `encrqcs::qcs_cache_dir_path()`.
  # @codedoc_comment_block news("encrqcs::qcs_cache_metadata_file_path", "2026-11-20", "1.2.0")

  # @codedoc_comment_block encrqcs::qcs_cache_metadata_file_path
  # `encrqcs::qcs_cache_metadata_file_path` returns the path to metadata file
  # in the cache.
  # It is simply `sprintf("%s/metadata.csv", qcs_cache_dir_path())`.
  # @codedoc_comment_block encrqcs::qcs_cache_metadata_file_path
  qcs_cache_metadata_file_path <- sprintf(
    "%s/metadata.csv",
    encrqcs::qcs_cache_dir_path()
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
  qcs_dir_path = NULL
) {
  # @codedoc_comment_block news("encrqcs::qcs_cache_metadata_read", "2025-06-10", "1.0.0")
  # New function `encrqcs::qcs_cache_metadata_read`.
  # @codedoc_comment_block news("encrqcs::qcs_cache_metadata_read", "2025-06-10", "1.0.0")
  # @codedoc_comment_block news("encrqcs::qcs_cache_metadata_read", "2026-11-20", "1.2.0")
  # `encrqcs::qcs_cache_metadata_read` now reads the metadata from
  # `encrqcs::qcs_cache_metadata_file_path()`. Argument `qcs_dir_path` is now
  # optional and is used to keep only the records of that QCS directory.
  # The metadata has a new column `qcs_dir_path`.
  # @codedoc_comment_block news("encrqcs::qcs_cache_metadata_read", "2026-11-20", "1.2.0")
  # @codedoc_comment_block encrqcs::qcs_cache_metadata_read
  # `encrqcs::qcs_cache_metadata_read` reads (with `[data.table::fread]`)
  # the file at `encrqcs::qcs_cache_metadata_file_path()`. If it
  # does not exist, an empty `data.table` is returned. Currently
  # the metadata file maps the combination of `qcs_dir_path`, `hash` and
  # `qcs_protocol_id` to `run_id`, where `hash` is computed on a dataset and
  # `run_id` corresponds to column `ID` in `temp/validation_run.csv` under
  # `qcs_dir_path`. This maps a dataset run with a specific protocol in a
  # specific QCS directory to a previous run reliably.
  # If `qcs_dir_path` is not `NULL`, only records with that (normalised)
  # `qcs_dir_path` are returned.
  # @codedoc_comment_block encrqcs::qcs_cache_metadata_read
  path <- encrqcs::qcs_cache_metadata_file_path()
  if (!file.exists(path)) {
    dt <- qcs_cache_metadata_empty__()
  } else {
    dt <- data.table::fread(
      path,
      encoding = "UTF-8",
      colClasses = c(qcs_dir_path = "character", hash = "character")
    )
  }
  if (!is.null(qcs_dir_path)) {
    qcs_dir_path <- normalize_qcs_dir_path__(qcs_dir_path)
    dt <- dt[dt[["qcs_dir_path"]] %in% qcs_dir_path, ]
  }
  return(dt[])
}

#' @eval codedoc::pkg_doc_fun(
#'   "encrqcs::qcs_cache_metadata_write",
#'   "qcs_cache"
#' )
qcs_cache_metadata_write <- function(
  cache_metadata_dt
) {
  # @codedoc_comment_block news("encrqcs::qcs_cache_metadata_write", "2025-06-10", "1.0.0")
  # New function `encrqcs::qcs_cache_metadata_write`.
  # @codedoc_comment_block news("encrqcs::qcs_cache_metadata_write", "2025-06-10", "1.0.0")
  # @codedoc_comment_block news("encrqcs::qcs_cache_metadata_write", "2026-11-20", "1.2.0")
  # `encrqcs::qcs_cache_metadata_write` no longer has argument
  # `qcs_dir_path`. `cache_metadata_dt` must now contain column
  # `qcs_dir_path` and it replaces the whole metadata file.
  # @codedoc_comment_block news("encrqcs::qcs_cache_metadata_write", "2026-11-20", "1.2.0")
  # @codedoc_comment_block encrqcs::qcs_cache_metadata_write
  # `encrqcs::qcs_cache_metadata_write` writes `cache_metadata_dt` with
  # `[data.table::fwrite]` into
  # `encrqcs::qcs_cache_metadata_file_path()`. This replaces the whole
  # metadata file, i.e. the metadata of all QCS directories.
  # @codedoc_comment_block encrqcs::qcs_cache_metadata_write
  #' @param cache_metadata_dt `[data.table]` (no default)
  #'
  #' Table of metadata to write into cache. Must have these columns:
  #' `c("metadata_datetime", "qcs_dir_path", "hash", "qcs_protocol_id", "run_id")`.
  dbc::assert_is_data_frame_with_required_names(
    x = cache_metadata_dt,
    required_names = c(
      "metadata_datetime", "qcs_dir_path", "hash", "qcs_protocol_id", "run_id"
    )
  )
  path <- encrqcs::qcs_cache_metadata_file_path()
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
    vr_dt <- data.table::data.table(ID = integer(0L))
  } else {
    vr_dt <- data.table::fread(file = vr_file_path)
    # @codedoc_comment_block news("qcs", "2026-09-10", "1.0.3")
    # `encrqcs` can now also read the `validation_run.csv` correctly for
    # QCS >= 2.4.0 where column `RUN_ID` -> `ID`.
    # @codedoc_comment_block news("qcs", "2026-09-10", "1.0.3")
    if ("RUN_ID" %in% names(vr_dt)) {
      # pre-2.4.0: RUN_ID. post: ID.
      data.table::setnames(vr_dt, "RUN_ID", "ID")
    }
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
  # The new record includes the normalised `qcs_dir_path`. Records of
  # `qcs_dir_path` whose `run_id` no longer exists in
  # `temp/validation_run.csv` are dropped.
  # @codedoc_comment_block encrqcs::qcs_cache_metadata_update
  qcs_dir_path <- normalize_qcs_dir_path__(qcs_dir_path)
  cache_metadata_dt <- encrqcs::qcs_cache_metadata_read()
  vr_dt <- qcs_cache_validation_run_file_read__(qcs_dir_path = qcs_dir_path)
  #' @param run_id `[integer]` (no default)
  #'
  #' One of the values of column `ID` in `temp/validation_run.csv` under
  #' the `qcs_dir_path`.
  dbc::assert_is_integer_nonNA_gtezero_atom(run_id)
  # drop records of this QCS dir whose runs no longer exist; keep records of
  # other QCS dirs as-is.
  cache_metadata_dt <- cache_metadata_dt[
    !cache_metadata_dt[["qcs_dir_path"]] %in% qcs_dir_path |
      cache_metadata_dt[["run_id"]] %in% vr_dt[["ID"]],
  ]
  #' @template param_optional_hash
  if (is.null(hash)) {
    #' @template param_optional_dataset_file_path
    hash <- qcs_cache_dataset_file_hash(dataset_file_path)
  }
  # @codedoc_comment_block news("encrqcs::qcs_cache_metadata_update", "2026-10-15", "1.1.0")
  # Used to overwrite the metadata with a new table with one row always.
  # Now retains the old metadata.
  # @codedoc_comment_block news("encrqcs::qcs_cache_metadata_update", "2026-10-15", "1.1.0")
  cache_metadata_dt <- rbind(
    cache_metadata_dt,
    data.table::data.table(
      metadata_datetime = Sys.time(),
      qcs_dir_path = qcs_dir_path,
      hash = hash,
      qcs_protocol_id = qcs_protocol_id,
      run_id = run_id
    )
  )
  data.table::setkeyv(cache_metadata_dt, "metadata_datetime")
  encrqcs::qcs_cache_metadata_write(
    cache_metadata_dt = cache_metadata_dt
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
  hash,
  qcs_protocol_id
) {
  dbc::assert_is_character_nonNA_atom(hash)
  qcs_protocol_id <- handle_arg_qcs_protocol_id__(qcs_protocol_id)
  # @codedoc_comment_block news("encrqcs::qcs_call", "2026-11-11", "1.1.2")
  # Cached results are now only re-used if both the dataset hash and
  # `qcs_protocol_id` match those of a previous run. Previously only the hash
  # was compared, so e.g. running the same dataset with a different
  # `qcs_protocol_id` was wrongly skipped. Also the latest matching run is now
  # used (previously the first) and only runs that still exist in
  # `temp/validation_run.csv` are considered.
  # @codedoc_comment_block news("encrqcs::qcs_call", "2026-11-11", "1.1.2")
  md_dt <- qcs_cache_metadata_read(qcs_dir_path = qcs_dir_path)
  vr_dt <- qcs_cache_validation_run_file_read__(qcs_dir_path = qcs_dir_path)
  idx <- which(
    md_dt[["hash"]] == hash &
      md_dt[["qcs_protocol_id"]] == qcs_protocol_id &
      md_dt[["run_id"]] %in% vr_dt[["ID"]]
  )
  if (length(idx) == 0L) {
    return(list())
  }
  # metadata rows are ordered by `metadata_datetime`; use the latest run.
  idx <- idx[length(idx)]
  out <- as.list(md_dt[idx, ])
  vr_dt_idx <- match(out[["run_id"]], vr_dt[["ID"]])
  out[["run_data"]] <- as.list(vr_dt[vr_dt_idx, ])
  return(out)
}

qcs_cache_has_results_for_dataset__ <- function(
  qcs_dir_path,
  hash,
  qcs_protocol_id
) {
  return(length(qcs_cache_metadata_for_dataset__(
    qcs_dir_path = qcs_dir_path,
    hash = hash,
    qcs_protocol_id = qcs_protocol_id
  )) > 0)
}
