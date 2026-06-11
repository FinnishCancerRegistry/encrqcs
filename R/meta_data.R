
#' @title JRC-ENCR QCS Metadata
#' @description
#' Tools to access metadata such as dataset names and column names of a dataset.
#' @name metadata
#' @eval c(
#'   "@details",
#'   codedoc::codedoc_lines("encrqcs::", text_file_paths = "R/meta_data.R")
#' )
NULL

.__QCS_DATASET_TEMPLATES <- list(
  incidence = data.table::data.table(
    PAT = "pat0000001",
    MoB = 1L,
    YoB = 1950L,
    Age = 70L,
    Sex = 1L,
    Geo_Code = "FI1B",
    Geo_Label = "Helsinki-Uusimaa",

    TUM = "tum0000001",
    MoI = 1L,
    YoI = 2000L,

    BoD = 1L,
    Topo = "C001",
    Morpho = 8070L,
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
    cT = NA_character_,
    cN = NA_character_,
    cM = NA_character_,
    pT = NA_character_,
    pN = NA_character_,
    pM = NA_character_,
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
qcs_dataset_template <- function(qcs_protocol_id, assertion_type = NULL) {
  # @codedoc_comment_block encrqcs::qcs_run_dataset_template
  # `[ecnrqcs::qcs_dataset_template]` returns a `data.table` with one row which
  # has the required columns in their required format for the given
  # `dataset_name`. Column names and classes taken from JRC-ENCR User
  # Compendium 2.0.
  # @codedoc_comment_block encrqcs::qcs_run_dataset_template
  # @codedoc_comment_block news("encrqcs::qcs_dataset_template", "2025-06-10", "1.0.0")
  # Replaced argument `dataset_name` with `qcs_protocol_id`.
  # @codedoc_comment_block news("encrqcs::qcs_dataset_template", "2025-06-10", "1.0.0")
  .__QCS_DATASET_TEMPLATES[[
    handle_arg_qcs_protocol_id__(qcs_protocol_id, output_type = "dataset_name")
  ]]
}

#' @rdname metadata
#' @export
qcs_dataset_column_names <- function(qcs_protocol_id) {
  # @codedoc_comment_block encrqcs::qcs_run_dataset_column_names
  # `[ecnrqcs::qcs_dataset_column_names]` returns a character string vector
  # of column names for the given `qcs_protocol_id`. This function wraps
  # `[ecnrqcs::qcs_dataset_template]`.
  # @codedoc_comment_block encrqcs::qcs_run_dataset_column_names
  qcs_protocol_id <- handle_arg_qcs_protocol_id__(
    qcs_protocol_id
  )
  return(names(encrqcs::qcs_dataset_template(qcs_protocol_id)))
}

#' @rdname metadata
#' @export
assert_is_qcs_dataset <- function(
  x,
  x_nm = NULL,
  call = NULL,
  assertion_type = NULL,
  qcs_protocol_id
) {
  #' @param x `[any class]` (no default)
  #'
  #' R object to perform assertion on.
  #' @param x_nm See `[dbc::handle_arg_x_nm]`
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  #' @param call See `[dbc::handle_arg_call]`
  call <- dbc::handle_arg_call(call)
  #' @param assertion_type See `[dbc::handle_arg_assertion_type]`
  assertion_type <- dbc::handle_arg_assertion_type(assertion_type)
  #' @template param_qcs_protocol_id
  qcs_protocol_id <- handle_arg_qcs_protocol_id__(qcs_protocol_id)
  dbc::assert_is_data_frame_with_required_names(
    x = x,
    x_nm = x_nm,
    call = call,
    assertion_type = assertion_type,
    required_names = encrqcs::qcs_dataset_column_names(qcs_protocol_id)
  )
  dataset_template <- encrqcs::qcs_dataset_template(qcs_protocol_id)
  lapply(encrqcs::qcs_dataset_column_names(qcs_protocol_id), function(col_nm) {
    dbc::assert_inherits(x = x[[col_nm]], x_nm = paste0(x_nm, "$", col_nm),
                         call = call,
                         required_class = class(dataset_template[[col_nm]])[1L])
  })
  invisible(NULL)
}
