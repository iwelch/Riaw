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

iaw$is.numeric <- function(nvec, required.length.of.nvec = NULL) {

    c1 <- is.numeric(nvec)
    if (is.null(required.length.of.nvec)) return(c1)

    stopifnot(is.numeric(required.length.of.nvec), length(required.length.of.nvec) == 1L)
    c1 && (length(nvec) == required.length.of.nvec)

}
