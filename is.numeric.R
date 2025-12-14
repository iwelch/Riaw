#' Test if Object is Numeric Vector of Specific Length
#'
#' @name is.numeric
#'
#' Extends base is.numeric to also check length.
#'
#' @param nvec Object to test.
#' @param required.length.of.nvec Required length (default 0).
#'
#' @return Logical scalar.
#'
#' @family type-checking
#' @export
#'
#' @examples
#' iaw$is.numeric(5, 1)
#' iaw$is.numeric(c(1, 2, 3), 3)

iaw$is.numeric <- function(nvec, required.length.of.nvec = 0) {
    stopifnot(is.numeric(required.length.of.nvec), length(required.length.of.nvec) == 1L)
    c1 <- .Primitive("is.numeric")(nvec)
    c2 <- (length(nvec) == required.length.of.nvec)
    c1 && c2
}
