#' @param hash `[character, NULL]` (default `NULL`)
#'
#' Optional hash (checksum) of file at `dataset_file_path`. See
#' `[qcs_cache_dataset_file_hash]`.
#'
#' - `NULL`: `hash` is computed first on `dataset_file_path`.
#' - `character`: Use this hash.
