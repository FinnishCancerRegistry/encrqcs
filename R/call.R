handle_arg_dataset_file_path__ <- function(
  dataset_file_path,
  must_exist = TRUE,
  assertion_type = NULL
) {
  if (must_exist) {
    dbc::assert_is_character_nonNA_atom(
      dataset_file_path,
      assertion_type = assertion_type
    )
    dbc::assert_file_exists(
      dataset_file_path,
      assertion_type = assertion_type
    )
  } else {
    dbc::assert_is_one_of(
      dataset_file_path,
      funs = list(dbc::report_is_NULL,
                  dbc::report_is_character_nonNA_atom),
      assertion_type = assertion_type
    )
  }
  if (!is.null(dataset_file_path)) {
    dataset_file_path <- normalizePath(
      dataset_file_path,
      winslash = "/",
      mustWork = FALSE
    )
  }
  return(dataset_file_path)
}

handle_arg_qcs_dir_path__ <- function(
  qcs_dir_path,
  assertion_type = NULL
) {
  dbc::assert_is_character_nonNA_atom(
    qcs_dir_path,
    assertion_type = assertion_type
  )
  dbc::assert_dir_exists(qcs_dir_path, assertion_type = assertion_type)
  if (length(dir(qcs_dir_path, pattern = "[.]jar$")) == 0) {
    stop("Supplied argument `qcs_dir_path` contains no .jar files: ",
         qcs_dir_path)
  }
  qcs_dir_path <- normalizePath(path = qcs_dir_path, winslash = "/")
  return(qcs_dir_path)
}

#' @title Run JRC-ENCR QCS
#' @description
#' Run JRC-ENCR QCS on a file on-disk.
#' @template param_mandatory_dataset_file_path
#' @template param_qcs_dir_path
#' @param system2_arg_list `[NULL, list]` (default `NULL`)
#'
#' Optional, additional arguments passed to `[system2]` if a list.
#' @template param_assertion_type
#' @eval codedoc::pkg_doc_fun("encrqcs::qcs_call")
#' @export
qcs_call <- function(
  dataset_file_path,
  qcs_dir_path,
  qcs_protocol_id,
  system2_arg_list = NULL,
  assertion_type = NULL,
  optional_steps = NULL
) {
  # @codedoc_comment_block encrqcs::qcs_call
  # Performs the following steps:
  #
  # - Run `optional_steps[["on_entry"]](env = eval_env)` if that element of
  #   `optional_steps` exists. `eval_env` is the evaluation environment of
  #   `encrqcs::qcs_call`.
  # @codedoc_comment_block encrqcs::qcs_call
  if ("on_entry" %in% names(optional_steps)) {
    optional_steps[["on_entry"]](env = eval_env)
  }
  # assertions -----------------------------------------------------------------
  dataset_file_path <- handle_arg_dataset_file_path__(
    dataset_file_path,
    assertion_type = assertion_type
  )

  # @codedoc_comment_block news("encrqcs::qcs_run", "2025-04-03", "0.6.0")
  # `encrqcs::qcs_call` gains argument `optional_steps`.
  # @codedoc_comment_block news("encrqcs::qcs_run", "2025-04-03", "0.6.0")
  #' @param optional_steps `[NULL, list]` (default `NULL`)
  #'
  #' Optional steps to perform during run.
  #'
  #' - `NULL`: No additional steps will be performed.
  #' - `list`: These will be performed. Each must be function with argument
  #'   `env`. The output of these functions is not used for anything --- but you
  #'   make changes to `env` directly.
  #'   See section **Functions** for the functions.
  dbc::assert_is_one_of(
    optional_steps,
    funs = list(dbc::report_is_NULL,
                dbc::report_is_uniquely_named_list)
  )
  eval_env <- environment()
  # @codedoc_comment_block encrqcs::qcs_call
  # - Run `on.exit(optional_steps[["on_exit"]](env = eval_env), add = TRUE)`
  #   if that element of
  #   `optional_steps` exists. E.g.
  #   have
  #   `optional_steps[["on_entry"]] = function(env) env$t <- proc.time()`
  #   and
  #   `optional_steps[["on_entry"]] = message(data.table::timetaken(env$t))`.
  # @codedoc_comment_block encrqcs::qcs_call
  if ("on_exit" %in% names(optional_steps)) {
    on.exit(optional_steps[["on_exit"]](env = eval_env), add = TRUE)
  }

  #' @template param_qcs_protocol_id
  qcs_protocol_id <- handle_qcs_protocol_id(qcs_protocol_id)

  dbc::assert_is_one_of(
    system2_arg_list,
    funs = list(dbc::report_is_NULL, dbc::report_is_list)
  )

  # hash -----------------------------------------------------------------------
  # @codedoc_comment_block news("encrqcs::qcs_run", "2025-06-10", "1.0.0")
  # `encrqcs::qcs_call` gains caching ability. It now saves the hash of every
  # dataset it has run through QCS and skips the run if the same dataset has
  # already been run previously.
  # @codedoc_comment_block news("encrqcs::qcs_run", "2025-06-10", "1.0.0")
  # @codedoc_comment_block encrqcs::qcs_call
  #  - See if a result set already exists for the dataset at `dataset_file_path`
  #    by computing its hash and comparing it to the hashes of previous runs.
  #    If such a result exists, return this early:
  #    `list(status = 0L, stdout = character(0L), stderr = character(0L), cache = TRUE)`.
  # @codedoc_comment_block encrqcs::qcs_call
  hash <- encrqcs::qcs_cache_dataset_file_hash(dataset_file_path)
  if (qcs_cache_has_results_for_dataset__(qcs_dir_path, hash)) {
    return(list(
      status = 0L, stdout = character(0L), stderr = character(0L), cache = TRUE
    ))
  }

  # system2 --------------------------------------------------------------------
  # @codedoc_comment_block encrqcs::qcs_call
  #  - R working directory is temporarily set via `[setwd]` to `qcs_dir_path`.
  #    Your original working directory is always restored whether the call
  #    succeeds or not.
  # @codedoc_comment_block encrqcs::qcs_call
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(qcs_dir_path)
  # @codedoc_comment_block news("encrqcs::qcs_run", "2025-02-11", "0.4.0")
  # `encrqcs::qcs_call` simplified. Instead of writing a .bit / .sh and
  # running that, it now directly calls Java via `system2`.
  # @codedoc_comment_block news("encrqcs::qcs_run", "2025-02-11", "0.4.0")
  # @codedoc_comment_block encrqcs::qcs_call
  #  - Determine arguments for the `[system2]` call. You may supply
  #    arguments to it via `system2_arg_list` and you can even override
  #    arguments determined by `encrqcs::qcs_call` --- any settings you pass
  #    that way overwrite the defaults determine by `encrqcs::qcs_call`.
  #    For instance, if you don't have the dir containing `java.exe` in your
  #    system `PATH` environment variables, the default call will fail,
  #    because it uses `command = "java"` --- but you can replace that with
  #    a direct path to your `java.exe` via `system2_arg_list`.
  #    The exact default for `system2_arg_list` is
  #    `${deparse1(encrqcs:::qcs_call_default_system2_arg_list_expr__())}`.
  # @codedoc_comment_block encrqcs::qcs_call
  jar_file_name <- dir(qcs_dir_path, pattern = "jrc-qcs[0-9.-]+.jar")
  default_system2_arg_list <- eval(
    qcs_call_default_system2_arg_list_expr__(),
    envir = list(
      jar_file_name = jar_file_name,
      qcs_protocol_id = qcs_protocol_id,
      dataset_file_path = dataset_file_path
    ),
    enclos = eval_env
  )
  user_system2_arg_list <- as.list(system2_arg_list)
  system2_arg_list <- default_system2_arg_list
  system2_arg_list[names(user_system2_arg_list)] <- user_system2_arg_list
  if (identical(system2_arg_list[["command"]], "java") && !has_java_cmd()) {
    stop("Command `java` not available on your system. ",
         "Either add it to your PATH or refer to the java executable directly ",
         "using argument `system2_arg_list`. See ?encrqcs::qcs_call and ",
         "?system2.")
  }
  # @codedoc_comment_block encrqcs::qcs_call
  # - Run `optional_steps[["pre_system2_call"]](env = eval_env)`
  #   if that element of
  #   `optional_steps` exists. E.g. to increase memory use for java to 10GB you
  #    can have
  #   `optional_steps[["pre_system2_call"]] =`
  #   `function(env) env$system2_arg_list$args[2] <- "-Xmx10g"`
  # @codedoc_comment_block encrqcs::qcs_call
  if ("pre_system2_call" %in% names(optional_steps)) {
    optional_steps[["pre_system2_call"]](env = eval_env)
  }
  # @codedoc_comment_block encrqcs::qcs_call
  # - Call `[system2]`.
  #   If status code other than zero is returned, an error is raised by R.
  #   E.g. status code 1 is "generic exit code" and means that the call
  #   failed.
  # - Add the hash of the dataset at `dataset_file_path` to the list of hashes
  #   in the cache so the next run on the identical dataset is skipped.
  # @codedoc_comment_block encrqcs::qcs_call
  vr_dt_pre <- qcs_cache_validation_run_file_read__(qcs_dir_path)
  out <- system2_call(system2_arg_list, success_status_codes = 0L)
  vr_dt_post <- qcs_cache_validation_run_file_read__(qcs_dir_path)
  new_run_id <- setdiff(vr_dt_post[["RUN_ID"]], vr_dt_pre[["RUN_ID"]])
  encrqcs::qcs_cache_metadata_update(
    qcs_dir_path = qcs_dir_path,
    hash = hash,
    qcs_protocol_id = qcs_protocol_id,
    run_id = new_run_id
  )
  out[["cache"]] <- FALSE

  # @codedoc_comment_block encrqcs::qcs_call
  # - Run `optional_steps[["post_system2_call"]](env = eval_env)`
  #   if that element of
  #   `optional_steps` exists.
  # @codedoc_comment_block encrqcs::qcs_call
  if ("post_system2_call" %in% names(optional_steps)) {
    optional_steps[["post_system2_call"]](env = eval_env)
  }
  # @codedoc_comment_block encrqcs::qcs_call
  #  - Finally,
  # @codedoc_insert_comment_block return(encrqcs::qcs_call)
  # @codedoc_comment_block encrqcs::qcs_call

  # @codedoc_comment_block return(encrqcs::qcs_call)
  #    Return a list containing the results of the `[system2]` call plus the
  #    additional element `cache = TRUE/FALSE` indicates whether the dataset at
  #    `dataset_file_path` had pre-existing results in the cache.
  # @codedoc_comment_block return(encrqcs::qcs_call)
  return(out)
}

has_java_cmd <- function() {
  suppressWarnings(
    system2("java", "-version", stdout = FALSE, stderr = FALSE) == 0L
  )
}

qcs_call_default_system2_arg_list_expr__ <- function() {
  quote(list(
    command = "java",
    args = c(
      "-jar",
      "-Xmx8g",
      jar_file_name,
      "-v", as.character(qcs_protocol_id),
      dataset_file_path
    )
  ))
}
