#' Test if Object is Numeric Vector of Specific Length
#'
#' @name is.numeric
#'
#' Extends base is.numeric to also check length.
#'
#' @param nvec Object to test.
#' @param required.length.of.nvec Required length, or \code{NULL} to skip
#'   length check (default \code{NULL}).
#'
#' @return Logical scalar.
#'
#' @family type-checking
#' @export
#'
#' @examples
#' # Basic numeric check (no length constraint)
#' iaw$is.numeric(c(1.5, 2.5, 3.5))
#'
#' # Length-constrained check: must be numeric AND exactly length 1
#' iaw$is.numeric(5, 1)
#' iaw$is.numeric(c(1, 2), 1)  # FALSE: wrong length
#'
#' # Useful for validating a fixed-length input
#' iaw$is.numeric(c(1, 2, 3), 3)
#'
#' # Difference from base::is.numeric: iaw version adds length check
#' is.numeric(c(1, 2, 3))         # TRUE (base, no length check)
#' iaw$is.numeric(c(1, 2, 3), 2)  # FALSE (iaw: wrong length)
#'
#' # A character vector is not numeric
#' iaw$is.numeric(c("1", "2", "3"))

iaw$is.numeric <- function(nvec, required.length.of.nvec = NULL) {

    c1 <- is.numeric(nvec)
    if (is.null(required.length.of.nvec)) return(c1)

    stopifnot(is.numeric(required.length.of.nvec), length(required.length.of.nvec) == 1L)
    c1 && (length(nvec) == required.length.of.nvec)

}
