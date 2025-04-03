

#' @title Run JRC-ENCR QCS
#' @description
#' Run JRC-ENCR QCS on a file on-disk.
#' @param dataset_file_path `[character]` (no default)
#'
#' Path to an existing file. This should be the dataset you want to use.
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
  dbc::assert_file_exists(dataset_file_path, assertion_type = assertion_type)
  dataset_file_path <- normalizePath(dataset_file_path, winslash = "/")
  dbc::assert_dir_exists(qcs_dir_path, assertion_type = assertion_type)
  dbc::assert_has_length(qcs_dir_path, expected_length = 1L,
                         assertion_type = assertion_type)
  qcs_dir_path <- normalizePath(path = qcs_dir_path, winslash = "/")

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

  # @codedoc_comment_block encrqcs::qcs_call::qcs_protocol_id
  # @param qcs_protocol_id `[character, integer]` (no default)
  #
  # - `integer`: Used as-is. You have to study what the `.jar` does with that.
  # - `character`: Take 11, 13, 15, or 17 for `"incidence"`, `"mortality"`,
  #   `"population"`, and `"lifetable"`, respectively.
  # @codedoc_comment_block encrqcs::qcs_call::qcs_protocol_id
  dbc::assert_is_one_of(
    qcs_protocol_id,
    funs = list(dbc::report_is_integer_nonNA_atom,
                dbc::report_is_character_nonNA_atom)
  )
  if (is.character(qcs_protocol_id)) {
    dbc::assert_atom_is_in_set(
      qcs_protocol_id,
      set = c("incidence", "mortality", "population", "lifetable")
    )
    qcs_protocol_id <- switch(
      qcs_protocol_id,
      incidence = 11L,
      mortality = 13L,
      population = 15L,
      lifetable = 17L,
      stop("No protocol id integer defined for qcs_protocol_id = ",
           deparse(qcs_protocol_id))
    )
  }

  dbc::assert_is_one_of(
    system2_arg_list,
    funs = list(dbc::report_is_NULL, dbc::report_is_list)
  )

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
  # @codedoc_comment_block encrqcs::qcs_call
  out <- system2_call(system2_arg_list, success_status_codes = 0L)
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
  #    Return a list containing the results of the `[system2]` call.
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
