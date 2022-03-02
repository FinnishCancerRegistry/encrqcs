

qcs_dataset_names <- function() {
  c("incidence", "mortality", "population", "lifetable")
}
qcs_dataset_column_names <- function(dataset_name) {
  # taken from JRC-ECNR QCS User Compendium 2.0
  switch(
    dataset_name,
    stop("Internal error: No column names defined for dataset_name = ",
         deparse(dataset_name)),
    incidence = c(
      "PAT", "MoB", "YoB", "Age", "Sex", "Geo_Code", "Geo_Label",
      "TUM", "MoI", "YoI", "BoD", "Topo", "Morpho", "Beh", "Grade",
      "Autopsy", "Vit_stat", "MoF", "YoF", "Surv_time",
      "ICD", "CoD", "TNM_ed",
      "cT", "cN", "cM", "pT", "pN", "pM", "ToS", "Stage",
      "Surgery", "Rt", "Cht", "Tt", "It", "Ht", "Ot", "SCT"
    ),
    mortality = c(
      "Calendar_Year", "Sex", "Age unit", "Cause of death", "Number of deaths"
    ),
    population = c(
      "Calendar Year", "Sex", "Age unit", "Geo_code", "Number of residents"
    ),
    lifetable = c(
      "Calendar Year", "Sex", "Annual age", "Geo_code", "All causes death probability"
    )
  )
}

assert_is_qcs_dataset_name <- function(
  x,
  x_nm = NULL,
  call = NULL,
  assertion_type = "input"
) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbC::handle_arg_call(call)
  dbc::assert_is_character_nonNA_atom(x, x_nm = x_nm, call = call)
  dbc::assert_atom_is_in_set(x, set = qcs_dataset_names(),
                             x_nm = x_nm, call = call)
}

assert_is_qcs_dataset <- function(
  x,
  x_nm = NULL,
  call = NULL,
  assertion_type = "input",
  dataset_name
) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbC::handle_arg_call(call)
  dbc::assert_is_data.frame_with_required_names(
    x = x,
    x_nm = x_nm,
    call = call,
    assertion_type = assertion_type,
    required_names = qcs_dataset_column_names(dataset_name)
  )
}






