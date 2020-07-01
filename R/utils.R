




#' @importFrom utils data maintainer
get_exported_dataset <- function(dataset_name) {
  stopifnot(
    length(dataset_name) == 1,
    is.character(dataset_name),
    !is.na(dataset_name)
  )

  expo_data_nms <- utils::data(package = "encrqcs")$results[, "Item"]
  if (!dataset_name %in% expo_data_nms) {
    stop(
      "Requested exported dataset ",
      deparse(dataset_name), " is not one of ",
      deparse(expo_data_nms), ". if you see this, complain to the package ",
      "maintainer: ", utils::maintainer("encrqcs")
    )
  }
  e <- new.env(parent = emptyenv())
  utils::data(list = dataset_name, envir = e)
  e[[dataset_name]]
}





#' @importFrom data.table copy
#' @importFrom utils maintainer
get_internal_dataset <- function(dataset_name) {
  stopifnot(
    length(dataset_name) == 1,
    is.character(dataset_name),
    !is.na(dataset_name)
  )
  pkg_env <- as.environment("package:encrqcs")
  object_nms <- getNamespaceExports("encrqcs")

  dataset_nms <- object_nms[vapply(object_nms, function(object_nm) {
    is.data.frame(pkg_env[[object_nm]])
  }, logical(1))]

  if (!dataset_name %in% dataset_nms) {
    stop(
      "Requested internal dataset ",
      deparse(dataset_name), " is not one of ",
      deparse(dataset_nms), ". if you see this, complain to the package ",
      "maintainer: ", utils::maintainer("encrqcs")
    )
  }

  data.table::copy(pkg_env[[dataset_name]])
}


