
#' Size of an R Object in MegaBytes
#'
#' @name object.size.MB
#'
#' provides a nice character description of the size of an object
#'
#' @usage object.size.MB(obj)
#' @param x an object
#'
#' @return
#'
#' @aliases object.size.mb


iaw$object.size.MB <- function (x) {
  gc()
  paste( round(as.numeric(object.size(x))/1024/1024,1), "MB")
}

iaw$object.size.mb <- iaw$object.size.MB
