system2_call <- function(
  arg_list,
  success_status_codes = NULL
) {
  dbc::assert_prod_input_is_uniquely_named_list(arg_list)
  dbc::assert_prod_input_is_one_of(
    success_status_codes,
    funs = list(dbc::report_is_NULL,
                dbc::report_is_integer_nonNA_vector)
  )
  tmp_out <- normalizePath(
    tempfile(pattern = "system2_stdout_", fileext = ".txt"),
    winslash = "/", mustWork = FALSE
  )
  writeLines(character(0L), tmp_out)
  tmp_err <- normalizePath(
    tempfile(pattern = "system2_stderr_", fileext = ".txt"),
    winslash = "/", mustWork = FALSE
  )
  writeLines(character(0L), tmp_err)
  on.exit({
    unlink(c(tmp_out, tmp_err), force = TRUE)
  })
  arg_list <- local({
    override_arg_list <- list(
      stdout = tmp_out,
      stderr = tmp_err
    )
    arg_list[names(override_arg_list)] <- override_arg_list
    arg_list
  })
  status <- do.call(system2, arg_list, quote = TRUE)
  out <- suppressWarnings(list(
    status = status,
    stdout = tryCatch(
      readLines(tmp_out, warn = FALSE),
      error = function(e) {
        paste0("encrqcs:::system2_call failed to read stdout: ",
               deparse1(e[["message"]]))
      }
    ),
    stderr = tryCatch(
      readLines(tmp_err, warn = FALSE),
      error = function(e) {
        paste0("encrqcs:::system2_call failed to read stderr: ",
               deparse1(e[["message"]]))
      }
    )
  ))
  return(out)
}
