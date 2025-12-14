#' Test if Object is Numeric Vector of Specific Length
#'
#' Extends base \code{is.numeric()} to also check that the vector has a
#' specific length. Useful for validating function arguments.
#'
#' @param nvec Object to test.
#' @param required.length.of.nvec Required length (default 0 means any length
#'   numeric vector passes if \code{nvec} is numeric).
#'
#' @return Logical: TRUE if \code{nvec} is numeric AND has exactly
#'   \code{required.length.of.nvec} elements.
#'
#' @export
#'
#' @seealso \code{\link{iaw$is.character}}, \code{\link{iaw$is.scalar}},
#'   \code{\link{is.numeric}}
#'
#' @examples
#' # Check for single number
#' iaw$is.numeric(5, 1)
#' # TRUE
#'
#' iaw$is.numeric(c(1, 2, 3), 3)
#' # TRUE
#'
#' iaw$is.numeric(c(1, 2, 3), 1)
#' # FALSE (wrong length)
#'
#' iaw$is.numeric("hello", 1)
#' # FALSE (not numeric)

iaw$is.numeric <- function(nvec, required.length.of.nvec = 0) {
    c1 <- .Primitive("is.numeric")(nvec)
    c2 <- length(nvec) == required.length.of.nvec
    c1 && c2
}
