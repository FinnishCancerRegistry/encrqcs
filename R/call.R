
qcs_script_extension <- function() {
  script_extension <- switch(
    get_os(), windows = ".bat", linux = ".sh", osx = ".sh"
  )
  return(script_extension)
}

qcs_script_name <- function() {
  paste0("R_package_encrqcs_temporary_script",
         qcs_script_extension())
}

qcs_script_path <- function(qcs_dir_path) {
  script_name <- qcs_script_name()
  script_path <- paste0(qcs_dir_path, "/", script_name)
  script_path <- normalizePath(script_path, mustWork = FALSE)
}

qcs_script_lines <- function(
  qcs_protocol_id,
  dataset_file_path,
  qcs_dir_path
) {
  jar_file_name <- dir(qcs_dir_path, pattern = "jrc-qcs[0-9.-]+.jar")
  if (length(jar_file_name) != 1L) {
    stop("Could not detect jrc-qcs-%VERSION%.jar in qcs_dir_path = ",
         deparse(qcs_dir_path), ". Either the dir you have supplied is not ",
         "the correct one or R package encrqcs needs to be fixed.")
  }
  cmd <- paste0("java -jar -Xmx2g ", jar_file_name,
                " -v %QCS_PROTOCOL_ID%",
                " %dataset_file_path%")
  replacements <- c(
    "%QCS_PROTOCOL_ID%" = qcs_protocol_id,
    "%dataset_file_path%" = normalizePath(dataset_file_path, winslash = "/")
  )
  for (i in seq_along(replacements)) {
    cmd <- gsub(names(replacements)[i], replacements[i], cmd)
  }

  script_head_tail <- switch(
    qcs_script_extension(),
    .bat = list(
      head = c(
        "REM This script written by R package encrqcs.",
        "REM It should be automatically deleted after execution finishes.",
        "SET OUTPUT_DIR=\\output"
      ),
      tail = c(":END", "")
    ),
    .sh = list(
      head = c(
        "#!/usr/bin/env bash",
        "",
        "# This script written by R package encrqcs.",
        "# It should be automatically deleted after execution finishes."
      ),
      tail = ""
    )
  )
  script_lines <- c(script_head_tail[["head"]], cmd, script_head_tail[["tail"]])
  return(script_lines)
}

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
#' @export
qcs_call <- function(
  dataset_file_path,
  qcs_dir_path,
  qcs_protocol_id,
  system2_arg_list = NULL,
  assertion_type = "input"
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
  # - `character`: Take 11, 3, 5, or 7 for `"incidence"`, `"mortality"`,
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

  # write script ---------------------------------------------------------------
  script_lines <- qcs_script_lines(
    qcs_protocol_id = qcs_protocol_id,
    dataset_file_path = dataset_file_path,
    qcs_dir_path = qcs_dir_path
  )
  script_path <- qcs_script_path(qcs_dir_path = qcs_dir_path)
  writeLines(script_lines, script_path)
  on.exit(unlink(script_path))

  # system2 --------------------------------------------------------------------
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(qcs_dir_path)
  message("encrqcs::qcs_call: executing script with these contents:\n",
          paste0("  ", script_lines, collapse = "\n"))
  system2_arg_list <- as.list(system2_arg_list)
  system2_arg_list[["command"]] <- qcs_script_name()
  out <- suppressWarnings(withCallingHandlers(
    do.call(system2, system2_arg_list, quote = TRUE),
    warning = function(w) w
  ))
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
      "system2 had status ", status, ". ",
      "Looks like the JRC-ENCR QCS Java ",
      "programme failed."
    )
    warning(warn_txt)
    if (is.character(out)) {
      msg_txt <- paste0(
        "system2 had status ", status, ". ",
        "Looks like the JRC-ENCR QCS Java ",
        "programme failed. ",
        "Got this from system2:\n",
        paste0("  ", out, collapse = "\n")
      )
      message(msg_txt)
    }
  }
  message("encrqcs::qcs_call: done.")
  return(out)
}

