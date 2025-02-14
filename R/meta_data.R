
#' @title JRC-ENCR QCS Metadata
#' @description
#' Tools to access metadata such as dataset names and column names of a dataset.
#' @name metadata
#' @eval c(
#'   "@details",
#'   codedoc::codedoc_lines("encrqcs::", text_file_paths = "R/meta_data.R")
#' )
NULL

#' @rdname metadata
#' @export
qcs_dataset_names <- function() {
  # @codedoc_comment_block encrqcs::qcs_run_dataset_names
  # `[ecnrqcs::qcs_dataset_names]` returns the set of allowed dataset names
  # as a character string vector.
  # @codedoc_comment_block encrqcs::qcs_run_dataset_names
  names(.__QCS_DATASET_TEMPLATES)
}

arg_dataset_name_docs <- function() {
  lines <- c(
    "@param dataset_name `[character]` (no default)",
    "",
    "one of the following:",
    paste0(" - \"", encrqcs::qcs_dataset_names(), "\"")
  )
  return(lines)
}

.__QCS_DATASET_TEMPLATES <- list(
  incidence = data.table::data.table(
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
  mortality = data.table::data.table(
    "Calendar year" = 2000L,
    "Sex" = 1L,
    "Age unit" = 1L,
    "Cause of death" = "C50",
    "Number of deaths" = 0L
  ),
  population = data.table::data.table(
    "Calendar year" = 2000L,
    "Sex" = 1L,
    "Age Unit" = 1L,
    # "Geo_code" = "FI1B",
    "Number of residents" = 100L
  ),
  lifetable = data.table::data.table(
    "Calendar year" = 2000L,
    "Sex" = 1L,
    "Age_Unit" = 1L,
    # "Geo_code" = "FI1B",
    "All causes death probability" = 0.01
  )
)


#' @rdname metadata
#' @export
#' @template param_assertion_type
#' @eval arg_dataset_name_docs()
qcs_dataset_template <- function(dataset_name, assertion_type = NULL) {
  encrqcs::assert_is_qcs_dataset_name(
    dataset_name,
    assertion_type = assertion_type
  )

  # @codedoc_comment_block encrqcs::qcs_run_dataset_template
  # `[ecnrqcs::qcs_dataset_template]` returns a `data.table` with one row which
  # has the required columns in their required format for the given
  # `dataset_name`. Column names and classes taken from JRC-ENCR User
  # Compendium 2.0.
  # @codedoc_comment_block encrqcs::qcs_run_dataset_template
  .__QCS_DATASET_TEMPLATES[[dataset_name]]
}

#' @rdname metadata
#' @export
qcs_dataset_column_names <- function(dataset_name, assertion_type = NULL) {
  # @codedoc_comment_block encrqcs::qcs_run_dataset_column_names
  # `[ecnrqcs::qcs_dataset_column_names]` returns a character string vector
  # of column names for the given `dataset_name`. This function wraps
  # `[ecnrqcs::qcs_dataset_template]`.
  # @codedoc_comment_block encrqcs::qcs_run_dataset_column_names
  encrqcs::assert_is_qcs_dataset_name(
    dataset_name,
    assertion_type = assertion_type
  )
  return(names(encrqcs::qcs_dataset_template(dataset_name)))
}

#' @rdname metadata
#' @export
#' @param x Assertion is performed on this object.
#' @param x_nm See e.g. `[dbc::dbc::assert_is_integer]`.
#' @param call See e.g. `[dbc::dbc::assert_is_integer]`.
assert_is_qcs_dataset_name <- function(
  x,
  x_nm = NULL,
  call = NULL,
  assertion_type = NULL
) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  dbc::assert_is_character_nonNA_atom(x, x_nm = x_nm, call = call)
  dbc::assert_atom_is_in_set(x, set = encrqcs::qcs_dataset_names(),
                             x_nm = x_nm, call = call)
}

#' @rdname metadata
#' @export
assert_is_qcs_dataset <- function(
  x,
  x_nm = NULL,
  call = NULL,
  assertion_type = NULL,
  dataset_name
) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  dbc::assert_is_data_frame_with_required_names(
    x = x,
    x_nm = x_nm,
    call = call,
    assertion_type = assertion_type,
    required_names = encrqcs::qcs_dataset_column_names(dataset_name)
  )
  dataset_template <- encrqcs::qcs_dataset_template(dataset_name)
  lapply(encrqcs::qcs_dataset_column_names(dataset_name), function(col_nm) {
    dbc::assert_inherits(x = x[[col_nm]], x_nm = paste0(x_nm, "$", col_nm),
                         call = call,
                         required_class = class(dataset_template[[col_nm]])[1L])
  })
  invisible(NULL)
}
