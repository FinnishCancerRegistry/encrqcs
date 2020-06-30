




generate_param_dataset_name_docs <- function() {

  lines <- c(
    "# these lines generated automatically; do not edit by hand!",
    "# instead, see function generate_param_dataset_name_docs",
    "#' @param dataset_name `[character]` (mandatory, no default)",
    "#'",
    "#' one of the following:",
    paste0("#' - \"", qcs_dataset_names(), "\"")
  )

  in_dev_mode <- dir.exists("man-roxygen")
  in_dev_mode <- in_dev_mode && "param_dataset_name.R" %in% dir("man-roxygen")
  if (in_dev_mode) {
    writeLines(lines, "man-roxygen/param_dataset_name.R")
  }

}
generate_param_dataset_name_docs()



