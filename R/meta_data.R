

qcs_dataset_names <- function() {
  c("incidence", "mortality", "population", "lifetable")
}

qcs_fake_dataset <- function(dataset_name) {
  assert_is_qcs_dataset_name(dataset_name, assertion_type = "prod_input")

  # Column names and classes taken from JRC-ENCR User Compendium 2.0
  switch(
    dataset,
    stop("Internal error: No fake dataset defined for dataset_name = ",
         deparse(dataset_name)),
    incidence = data.frame(
      PAT = 1L,
      MoB = 1L,
      YoB = 1950L,
      Age = 70L,
      Sex = 1L,
      Geo_Code = "FI1B",
      Geo_Label = "Helsinki-Uusimaa",

      TUM = 1L,
      MoI = 1L,
      YoI = 2000L,

      BoD = 1L,
      Topo = "C001",
      Morpho = "8070",
      Beh = 3L,
      Grade = 2L,

      Autopsy = 0L,
      Vit_stat = 1L,
      MoF = 1L,
      YoF = 2020L,
      Surv_time = 7305L,
      ICD = 10L,
      CoD = NA_character_,

      TNM_ed = 8L,
      cT = NA_integer_,
      cN = NA_integer_,
      cM = NA_integer_,
      pT = NA_integer_,
      pN = NA_integer_,
      pM = NA_integer_,
      ToS = "cpS",
      Stage = 2L,

      Surgery = 0L,
      Rt = 0L,
      Cht = 0L,
      Tt = 0L,
      It = 0L,
      Ht = 0L,
      Ot = 0L,
      SCT = 0L
    ),
    mortality = data.frame(
      "Calendar_Year" = 2000L,
      "Sex" = 1L,
      "Age unit" = 1L,
      "Cause of death" = "C50",
      "Number of deaths" = 0L
    ),
    population = data.frame(
      "Calendar Year" = 2000L,
      "Sex" = 1L,
      "Age unit" = 1L,
      "Geo_code" = "FI1B",
      "Number of residents" = 100L
    ),
    lifetable = data.frame(
      "Calendar Year" = 2000L,
      "Sex" = 1L,
      "Annual age" = 1L,
      "Geo_code" = "FI1B",
      "All causes death probability" = 0.01
    )
  )
}

qcs_dataset_column_names <- function(dataset_name) {
  assert_is_qcs_dataset_name(dataset_name, assertion_type = "prod_input")
  return(names(qcs_fake_dataset(dataset_name)))
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
  fake_dataset <- qcs_fake_dataset(dataset_name)
  lapply(qcs_dataset_column_names(dataset_name), function(col_nm) {
    dbc::assert_inherits(x = x[[col_nm]], x_nm = paste0(x_nm, "$", col_nm),
                         call = call,
                         required_class = class(fake_dataset[[col_nm]])[1L])
  })
  invisible(NULL)
}






