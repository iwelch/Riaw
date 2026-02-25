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
#' iaw$is.scalar(5)          # TRUE
#' iaw$is.scalar(c(1, 2))    # FALSE: length > 1
#'
#' # Single string is a scalar
#' iaw$is.scalar("hello")    # TRUE
#'
#' # NULL and zero-length vectors are not scalars
#' iaw$is.scalar(NULL)       # FALSE
#' iaw$is.scalar(integer(0)) # FALSE
#'
#' # Lists are not atomic, so not scalar
#' iaw$is.scalar(list(1))    # FALSE

iaw$is.scalar <- function(x) {
    is.atomic(x) && length(x) == 1L
}
