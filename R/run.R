
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
  qcs_version,
  qcs_protocol_id,
  input_file_path
) {
  cmd <- paste0("java -jar -Xmx2g jrc-qcs-%QCS_VERSION%.jar",
                " -v %QCS_PROTOCOL_ID% %INPUT_FILE_PATH%")
  replacements <- c(
    "%QCS_VERSION%"     = qcs_version,
    "%QCS_PROTOCOL_ID%" = qcs_protocol_id,
    "%INPUT_FILE_PATH%" = input_file_path
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

qcs_run <- function(
  qcs_dir_path,
  qcs_version = "2.0",
  qcs_protocol_id = 11L,
  assertion_type = "input"
) {
  dbc::assert_dir_exists(qcs_dir_path, assertion_type = assertion_type)
  dbc::assert_has_length(qcs_dir_path, expected_length = 1L,
                         assertion_type = assertion_type)
  dbc::assert_is_character_nonNA_atom(
    qcs_version, assertion_type = assertion_type
  )
  dbc::assert_is_integer_nonNA_atom(
    qcs_protocol_id, assertion_type = assertion_type
  )

  script_path <- qcs_script_path()
  writeLines(script_lines, script_file_path)
  on.exit(unlink(script_file_path))

  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(qcs_dir_path)
  system2(qcs_script_name())

}

