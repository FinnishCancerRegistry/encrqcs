dataset_name_to_qcs_protocol_id_map__ <- c(
  incidence = 11L,
  mortality = 13L,
  population = 15L,
  lifetable = 17L
)

#' @rdname metadata
#' @export
assert_is_qcs_protocol_id <- function(
  x,
  x_nm = NULL,
  call = NULL,
  assertion_type = NULL
) {
  dbc::handle_args_inplace()
  dbc::assert_is_one_of(
    x = x,
    x_nm = x_nm,
    call = call,
    assertion_type = assertion_type,
    funs = list(
      dbc::report_is_integer_nonNA_atom,
      dbc::report_is_character_nonNA_atom
    )
  )
  if (is.character(x)) {
    dbc::assert_atom_is_in_set(
      x = x,
      x_nm = x_nm,
      call = call,
      assertion_type = assertion_type,
      set = names(dataset_name_to_qcs_protocol_id_map__)
    )
  } else {
    # @codedoc_comment_block news("encrqcs", "2026-09-16", "1.0.4")
    # `qcs_protocol_id` can now be any integer larger than zero.
    # @codedoc_comment_block news("encrqcs", "2026-09-16", "1.0.4")
    dbc::assert_is_integer_nonNA_gtzero_atom(
      x = x,
      x_nm = x_nm,
      call = call,
      assertion_type = assertion_type
    )
  }
}

handle_arg_qcs_protocol_id__ <- function(
  qcs_protocol_id,
  output_type = "id"
) {
  assert_is_qcs_protocol_id(qcs_protocol_id)
  if (output_type == "id" && is.character(qcs_protocol_id)) {
    qcs_protocol_id <- dataset_name_to_qcs_protocol_id_map__[[qcs_protocol_id]]
  } else if (output_type == "dataset_name" && is.integer(qcs_protocol_id)) {
    qcs_protocol_id <- names(dataset_name_to_qcs_protocol_id_map__)[
      dataset_name_to_qcs_protocol_id_map__ == qcs_protocol_id
    ]
  }
  return(qcs_protocol_id)
}
