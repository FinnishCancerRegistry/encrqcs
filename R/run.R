



#' @title JRC-ENCR QCS
#' @description
#' Run JRC-ENCR QCS and read results into R.
#' @template param_dataset
#' @eval c(
#'   arg_dataset_name_docs(),
#'   codedoc::codedoc_lines("^encrqcs::qcs_run", "R/run.R")
#' )
#' @template param_qcs_dir_path
#' @template param_assertion_type
#' @export
qcs_run <- function(
    dataset,
    dataset_name,
    qcs_dir_path,
    dataset_file_path = NULL,
    clean             = NULL,
    write_arg_list    = NULL,
    run_arg_list      = NULL,
    read_arg_list     = NULL,
    assertion_type    = "input"
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
  # @codedoc_comment_block encrqcs::qcs_run::clean
  # @param clean `[NULL, logical]` (default `NULL`)
  #
  # One of the following:
  # - `NULL`     : Use `"both"`.
  # - `"input"`  : Delete the file in `dataset_file_path` when this function
  #                no longer needs it.
  # - `"output"` : Delete output files when this function no longer need them.
  #                Specifically, the whole output directory for the given
  #                `dataset_name` is removed, e.g. `output/incidence`,
  #                in `qcs_dir_path`.
  # - `"both"`   : Delete both input and output files/dirs.
  # - `"neither"`: Don't delete anything.
  # @codedoc_comment_block encrqcs::qcs_run::clean
  dbc::assert_is_one_of(
    clean,
    funs = list(dbc::report_is_NULL, dbc::report_is_character_nonNA_atom),
    assertion_type = assertion_type
  )
  if (is.null(clean)) {
    clean <- "both"
  }
  dbc::assert_atom_is_in_set(
    clean, set = c("input", "output", "both", "neither"),
    assertion_type = assertion_type
  )

  # write ----------------------------------------------------------------------
  # @codedoc_comment_block encrqcs::qcs_run::write_arg_list
  # @param write_arg_list `[NULL, list]` (default `NULL`)
  #
  # Optional, additional arguments passed to `[encrqcs::qcs_write_dataset]`
  # if a list. Arguments `dataset`, `dataset_name`, `file_path`, and
  # `assertion` type are determined internally and cannot be changed.
  # @codedoc_comment_block encrqcs::qcs_run::write_arg_list

  # @codedoc_comment_block encrqcs::qcs_run::dataset_file_path
  # @param dataset_file_path `[NULL, character]` (default `NULL`)
  #
  # `dataset` needs to be written to hard drive for use by JRC-ENCR QCS.
  # You can (optionally) specify the path where to write `dataset` via this
  # arg.
  #
  # - `NULL`: `dataset` will be written into a temporary file given by
  #   `tempfile(fileext = ".csv")`.
  # - `character`: `dataset` will be written here.
  # @codedoc_comment_block encrqcs::qcs_run::dataset_file_path
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
  # @codedoc_comment_block details(encrqcs::qcs_run)
  # `[encrqcs::qcs_run]` performs the following steps:
  #
  # 1. `[encrqcs::qcs_write_dataset]` is called to write `dataset` to disk
  #    (see arg `write_arg_list`).
  # @codedoc_comment_block details(encrqcs::qcs_run)
  do.call(encrqcs::qcs_write_dataset, write_arg_list, quote = TRUE)
  if (clean %in% c("input", "both")) {
    on.exit(unlink(dataset_file_path, force = TRUE))
  }

  # run ------------------------------------------------------------------------
  # @codedoc_comment_block encrqcs::qcs_run::run_arg_list
  # @param run_arg_list `[NULL, list]` (default `NULL`)
  #
  # Optional, additional arguments passed to `[encrqcs::qcs_run_call]`
  # if a list. Arguments `dataset_file_path`, `qcs_dir_path`,
  # `assertion_type`, and `system2_arg_list` are determined internally and
  # cannot be changed.
  # @codedoc_comment_block encrqcs::qcs_run::run_arg_list
  overriding_run_arg_list <- list(
    dataset_file_path = dataset_file_path,
    qcs_dir_path = qcs_dir_path,
    assertion_type = assertion_type,
    system2_arg_list = list(stdout = TRUE, stderr = TRUE)
  )
  run_arg_list <- as.list(run_arg_list)
  run_arg_list[names(overriding_run_arg_list)] <- overriding_run_arg_list
  # @codedoc_comment_block details(encrqcs::qcs_run)
  # 2. `[encrqcs::qcs_run_call]` is called to run checks on the on-disk dataset
  #    (see arg `run_arg_list`). Any messages that JRC-ENCR QCS emits are
  #    captured into acharacter string vector which will be included in
  #    the ouptut of `[encrqcs::qcs_run]`.
  # @codedoc_comment_block details(encrqcs::qcs_run)
  run_log <- do.call(encrqcs::qcs_call, run_arg_list, quote = TRUE)

  # read -----------------------------------------------------------------------
  # @codedoc_comment_block encrqcs::qcs::read_arg_list
  # @param read_arg_list `[NULL, list]` (default `NULL`)
  # Optional, additional arguments passed to `[encrqcs::qcs_read_results]`
  # if a list. Arguments `qcs_dir_path`, `dataset_name`, and
  # `assertion_type` are determined internally and
  # cannot be changed.
  # @codedoc_comment_block encrqcs::qcs::read_arg_list
  overriding_read_arg_list <- list(
    qcs_dir_path = qcs_dir_path,
    dataset_name = dataset_name,
    assertion_type = assertion_type
  )
  read_arg_list <- as.list(read_arg_list)
  read_arg_list[names(overriding_read_arg_list)] <- overriding_read_arg_list
  # @codedoc_comment_block details(encrqcs::qcs)
  # 3. `[encrqcs::qcs_read_results]` is called to read results back into R.
  # @codedoc_comment_block details(encrqcs::qcs)
  output <- do.call(encrqcs::qcs_read_results, read_arg_list, quote = TRUE)

  # finishing touches ----------------------------------------------------------
  # @codedoc_comment_block details(encrqcs::qcs)
  # 4. The captured messages alluded to in step 2 are included in the output
  #    as element named `run_log`.
  # @codedoc_comment_block details(encrqcs::qcs)
  output[["run_log"]] <- run_log
  if (clean %in% c("output", "both")) {
    output_dir_path <- qcs_read_dir_path(
      qcs_dir_path = qcs_dir_path, dataset_name = dataset_name
    )
    # @codedoc_comment_block details(encrqcs::qcs)
    # 5. Input / output files are removed on exit (whether successful or not)
    #    of `[encrqcs::qcs]` depending on arg `clean`.
    # @codedoc_comment_block details(encrqcs::qcs)
    on.exit(unlink(output_dir_path, force = TRUE, recursive = TRUE),
            add = TRUE)
  }

  return(output)
}

