




arg_dataset_name_docs <- function() {
  lines <- c(
    "#' @param dataset_name `[character]` (no default)",
    "#'",
    "#' one of the following:",
    paste0("#' - \"", qcs_dataset_names(), "\"")
  )
  return(lines)
}



