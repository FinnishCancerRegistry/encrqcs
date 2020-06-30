





qcs_dataset_names <- function() {
  c("incidence")
}


assert_is_qcs_dataset_name <- function(dataset_name) {
  dbc::assert_is_character_nonNA_atom(dataset_name)
  dbc::assert_atom_is_in_set(dataset_name, set = qcs_dataset_names())
}

assert_is_qcs_dataset <- function(dataset, dataset_name) {
  assert_is_qcs_dataset_name(dataset_name)
  dbc::assert_is_data.frame_with_required_names(
    x = dataset,
    x_nm = "dataset",
    required_names = qcs_column_names(dataset_name)
  )
}



qcs_column_names <- function(dataset_name = "incidence", core = NA) {
  assert_is_qcs_dataset_name(dataset_name)
  dbc::assert_is_logical_atom(core)
  core_values <- "Y"
  if (identical(core, FALSE)) {
    core_values <- "N"
  } else if (identical(core, NA)) {
    core_values <- c("Y", "N")
  }

  dt <- get_internal_dataset("column_specifications")
  dt[["name"]][dt[["core"]] %in% core_values]
}



#' @title Column Specifications
#' @description
#' Retrieve a table of column specifications for the reqested dataset name.
#' @template param_dataset_name
#' @export
qcs_column_specifications <- function(dataset_name = "incidence") {
  assert_is_qcs_dataset_name(dataset_name)
  switch(
    dataset_name,
    stop("no specs defined for dataset_name = ", deparse(dataset_name)),
    incidence = get_internal_dataset("column_specifications")
  )
}






