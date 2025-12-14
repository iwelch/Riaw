#' Test if Object is a Scalar
#'
#' @name is.scalar
#'
#' Checks whether an object is a single atomic value.
#'
#' @param x Object to test.
#'
#' @return Logical scalar.
#'
#' @family type-checking
#' @export
#'
#' @examples
#' iaw$is.scalar(5)
#' iaw$is.scalar(c(1, 2))

iaw$is.scalar <- function(x) {
    is.atomic(x) && length(x) == 1L
}
