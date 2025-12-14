#' Test if Object is a Scalar (Single Atomic Value)
#'
#' Checks whether an object is a single atomic value (length 1). Useful for
#' validating that arguments are single values, not vectors.
#'
#' @param x Object to test.
#'
#' @return Logical: TRUE if \code{x} is atomic and has length 1.
#'
#' @export
#'
#' @seealso \code{\link{iaw$is.numeric}}, \code{\link{iaw$is.character}},
#'   \code{\link{is.atomic}}
#'
#' @examples
#' iaw$is.scalar(5)
#' # TRUE
#'
#' iaw$is.scalar("hello")
#' # TRUE
#'
#' iaw$is.scalar(TRUE)
#' # TRUE
#'
#' iaw$is.scalar(c(1, 2))
#' # FALSE (length > 1)
#'
#' iaw$is.scalar(list(a = 1))
#' # FALSE (list, not atomic)
#'
#' iaw$is.scalar(NULL)
#' # FALSE (length 0)

iaw$is.scalar <- function(x) {
    is.atomic(x) && length(x) == 1L
}
