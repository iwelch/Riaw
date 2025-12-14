#' Object Size in MB
#'
#' @name object.size.MB
#'
#' Returns object size in megabytes.
#'
#' @param x Object to measure.
#'
#' @return Size in MB.
#'
#' @family utilities
#' @export
#'
#' @examples
#' iaw$object.size.MB(1:1000000)

iaw$object.size.MB <- function(x) {
    round(object.size(x) / 1024^2, 2)
}
