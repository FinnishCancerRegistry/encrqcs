



#' @title JRC-ENCR QCS
#' @description
#' Run JRC-ENCR QCS and read results into R.
#' @template param_dataset
#' @eval arg_dataset_name_docs()
#' @template param_qcs_dir_path
#' @param dataset_file_path `[NULL, character]` (default `NULL`)
#'
#' `dataset` needs to be written to hard drive for use by JRC-ENCR QCS.
#' You can (optionally) specify the path where to write `dataset` via this
#' arg.
#'
#' - `NULL`: `dataset` will be written into a temporary file given by
#'   `tempfile(fileext = ".csv")`.
#' - `character`: `dataset` will be written here.
#'
#' @param clean `[logical]` (default `"both"`)
#'
#' One of the following:
#' - `"input"`  : Delete input file when this function no longer needs it.
#' - `"output"` : Delete output files when this function no longer need them.
#' - `"both"`   : Delete both input and output files.
#' - `"neither"`: Don't delete any files.
#' @param write_arg_list `[NULL, list]` (default `NULL`)
#'
#' Optional, additional arguments passed to `[encrqcs::qcs_write_dataset]`
#' if a list.
#' @param run_arg_list `[NULL, list]` (default `NULL`)
#'
#' Optional, additional arguments passed to `[encrqcs::qcs_run]`
#' if a list.
#' @param read_arg_list `[NULL, list]` (default `NULL`)
#'
#' Optional, additional arguments passed to `[encrqcs::qcs_read_results]`
#' if a list.
#' @template param_assertion_type
qcs <- function(
    dataset,
    dataset_name,
    qcs_dir_path,
    dataset_file_path = NULL,
    clean = c("input", "output", "both", "neither")[3L],
    write_arg_list = NULL,
    run_arg_list = NULL,
    read_arg_list = NULL,
    assertion_type = "input"
) {
  # assertions -----------------------------------------------------------------
  encrqcs::assert_is_qcs_dataset(dataset, dataset_name = dataset_name,
                                 assertion_type = assertion_type)
  encrqcs::assert_is_qcs_dataset_name(dataset_name,
                                      assertion_type = assertion_type)
  dbc::assert_dir_exists(qcs_dir_path,
                         assertion_type = assertion_type)
  dbc::assert_is_one_of(
    dataset_file_path,
    funs = list(
      dbc::report_is_NULL,
      dbc::report_is_character_nonNA_atom
    ),
    assertion_type = assertion_type
  )
  dbc::assert_is_character_nonNA_atom(clean,
                                      assertion_type = assertion_type)
  dbc::assert_atom_is_in_set(
    clean, set = c("input", "output", "both", "neither"),
    assertion_type = assertion_type
  )

  # write ----------------------------------------------------------------------
  if (is.null(dataset_file_path)) {
    dataset_file_path <- tempfile(fileext = ".csv")
  }
  overriding_write_arg_list <- list(
    dataset = dataset,
    dataset_name = dataset_name,
    file_path = dataset_file_path,
    assertion_type = assertion_type
  )
  write_arg_list <- as.list(write_arg_list)
  write_arg_list[names(overriding_write_arg_list)] <- overriding_write_arg_list
  do.call(encrqcs::qcs_write_dataset, write_arg_list, quote = TRUE)
  if (clean %in% c("input", "both")) {
    on.exit(unlink(dataset_file_path, force = TRUE))
  }

  # run ------------------------------------------------------------------------
  overriding_run_arg_list <- list(
    dataset_file_path = dataset_file_path,
    qcs_dir_path = qcs_dir_path,
    assertion_type = assertion_type,
    system2_arg_list = list(stdout = TRUE, stderr = TRUE)
  )
  run_arg_list <- as.list(run_arg_list)
  run_arg_list[names(overriding_run_arg_list)] <- overriding_run_arg_list
  run_log <- do.call(encrqcs::qcs_run, run_arg_list, quote = TRUE)

  # read -----------------------------------------------------------------------
  overriding_read_arg_list <- list(
    qcs_dir_path = qcs_dir_path,
    dataset_name = dataset_name,
    assertion_type = assertion_type
  )
  read_arg_list <- as.list(read_arg_list)
  read_arg_list[names(overriding_read_arg_list)] <- overriding_read_arg_list
  output <- do.call(encrqcs::qcs_read_results, read_arg_list, quote = TRUE)

  # finishing touches ----------------------------------------------------------
  output[["run_log"]] <- run_log
  if (clean %in% c("output", "both")) {
    output_dir_path <- qcs_read_dir_path(
      qcs_dir_path = qcs_dir_path, dataset_name = dataset_name
    )
    on.exit(unlink(output_dir_path, force = TRUE, recursive = TRUE),
            add = TRUE)
  }

  return(output)
}

