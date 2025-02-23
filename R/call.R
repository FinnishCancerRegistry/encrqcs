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
#' @eval c(
#'   codedoc::codedoc_lines("^encrqcs::qcs_call::"),
#'   "@details",
#'   codedoc::codedoc_lines("^\\Qdetails(encrqcs::qcs_call)\\E$"),
#'   "@return",
#'   codedoc::codedoc_lines("^\\Qreturn(encrqcs::qcs_call)\\E$")
#' )
#' @export
qcs_call <- function(
  dataset_file_path,
  qcs_dir_path,
  qcs_protocol_id,
  system2_arg_list = NULL,
  assertion_type = NULL
) {
  # assertions -----------------------------------------------------------------
  dbc::assert_file_exists(dataset_file_path, assertion_type = assertion_type)
  dataset_file_path <- normalizePath(dataset_file_path, winslash = "/")
  dbc::assert_dir_exists(qcs_dir_path, assertion_type = assertion_type)
  dbc::assert_has_length(qcs_dir_path, expected_length = 1L,
                         assertion_type = assertion_type)

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
  # @codedoc_comment_block details(encrqcs::qcs_call)
  #  - R working directory is temporarily set via `[setwd]` to `qcs_dir_path`.
  #    Your original working directory is always restored whether the call
  #    succeeds or not.
  # @codedoc_comment_block details(encrqcs::qcs_call)
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(qcs_dir_path)
  # @codedoc_comment_block news("encrqcs::qcs_run", "2025-02-11", "0.4.0")
  # `encrqcs::qcs_call` simplified. Instead of writing a .bit / .sh and
  # running that, it now directly calls Java via `system2`.
  # @codedoc_comment_block news("encrqcs::qcs_run", "2025-02-11", "0.4.0")
  # @codedoc_comment_block details(encrqcs::qcs_call)
  #  - The Java programme is called via a `system2` call. You may supply
  #    arguments to it via `system2_arg_list` and you can even override
  #    arguments determined by `encrqcs::qcs_call` --- any settings you pass
  #    that way overwrite the defaults determine by `encrqcs::qcs_call`.
  #    For instance, if you don't have the dir containing `java.exe` in your
  #    system `PATH` environment variables, the default call will fail,
  #    because it uses `command = "java"` --- but you can replace that with
  #    a direct path to your `java.exe` via `system2_arg_list`.
  # @codedoc_comment_block details(encrqcs::qcs_call)
  jar_file_name <- dir(qcs_dir_path, pattern = "jrc-qcs[0-9.-]+.jar")
  default_system2_arg_list <- list(
    command = "java",
    args = c(
      "-jar",
      "-Xmx2g",
      jar_file_name,
      "-v", as.character(qcs_protocol_id),
      dataset_file_path
    )
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
  out <- suppressWarnings(withCallingHandlers(
    do.call(system2, system2_arg_list, quote = TRUE),
    warning = function(w) w
  ))
  # @codedoc_comment_block details(encrqcs::qcs_call)
  #  - If status code other than zero is reported, a warning is emitted by R.
  #    E.g. status code 1 is "generic exit code" and means that the call
  #    failed.
  # @codedoc_comment_block details(encrqcs::qcs_call)
  status <- 0L
  if (is.integer(out)) {
    status <- out
  } else if (is.integer(attr(out, "status"))) {
    status <- attr(out, "status")
  }
  had_status_1 <- status > 0L
  had_status_1 <- had_status_1 || inherits(out, "warning") &&
    grepl("had status 1", out[["message"]])
  if (had_status_1) {
    warn_txt <- paste0(
      "system2 returneds status ", status, ". ",
      "Looks like the JRC-ENCR QCS Java ",
      "programme failed."
    )
    warning(warn_txt)
    if (is.character(out)) {
      msg_txt <- paste0(
        "system2 returned status ", status, ". ",
        "Looks like the JRC-ENCR QCS Java ",
        "programme failed. ",
        "Got this from system2:\n",
        paste0("  ", out, collapse = "\n")
      )
      message(msg_txt)
    }
  }
  # @codedoc_comment_block details(encrqcs::qcs_call)
  #  - Finally,
  # @codedoc_insert_comment_block return(encrqcs::qcs_call)
  # @codedoc_comment_block details(encrqcs::qcs_call)

  # @codedoc_comment_block return(encrqcs::qcs_call)
  #    The output of the `[system2]` call is returned as-is.
  # @codedoc_comment_block return(encrqcs::qcs_call)
  message("encrqcs::qcs_call: done.")
  return(out)
}

has_java_cmd <- function() {
  suppressWarnings(
    system2("java", "-version", stdout = FALSE, stderr = FALSE) == 0L
  )
}
