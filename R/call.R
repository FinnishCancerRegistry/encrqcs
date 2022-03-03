
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
        "@ECHO off",
        "",
        "REM this script written by R package encrqcs.",
        "REM it should be automatically deleted after execution finishes.",
        "SET OUTPUT_DIR=\\output"
      ),
      tail = c(":END", "")
    ),
    .sh = list(
      head = c(
        "#!/usr/bin/env bash",
        "",
        "# this script written by R package encrqcs.",
        "# it should be automatically deleted after execution finishes."
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
#' @param qcs_protocol_id `[integer]` (default `11L`)
#'
#' See the JRC-ENCR QCS User Compendium.
#' @param system2_arg_list `[NULL, list]` (default `NULL`)
#'
#' Optional, additional arguments passed to `[system2]` if a list.
#' @template param_assertion_type
#' @export
qcs_call <- function(
  dataset_file_path,
  qcs_dir_path,
  qcs_protocol_id = 11L,
  system2_arg_list = NULL,
  assertion_type = "input"
) {
  dbc::assert_file_exists(dataset_file_path, assertion_type = assertion_type)
  dataset_file_path <- normalizePath(dataset_file_path, winslash = "/")
  dbc::assert_dir_exists(qcs_dir_path, assertion_type = assertion_type)
  dbc::assert_has_length(qcs_dir_path, expected_length = 1L,
                         assertion_type = assertion_type)
  dbc::assert_is_integer_nonNA_atom(
    qcs_protocol_id, assertion_type = assertion_type
  )
  dbc::assert_is_one_of(
    system2_arg_list,
    funs = list(dbc::report_is_NULL, dbc::report_is_list)
  )

  script_lines <- qcs_script_lines(
    qcs_protocol_id = qcs_protocol_id,
    dataset_file_path = dataset_file_path,
    qcs_dir_path = qcs_dir_path
  )
  script_path <- qcs_script_path(qcs_dir_path = qcs_dir_path)
  writeLines(script_lines, script_path)
  on.exit(unlink(script_path))

  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(qcs_dir_path)
  message("encrqcs::qcs_call: executing this:\n",
          paste0("  ", script_lines, collapse = "\n"))
  system2_arg_list <- as.list(system2_arg_list)
  system2_arg_list[["command"]] <- qcs_script_name()
  out <- do.call(system2, system2_arg_list, quote = TRUE)
  message("encrqcs::qcs_call: done.")
  return(out)
}

