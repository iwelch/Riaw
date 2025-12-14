#' Standardize a Vector
#'
#' @name standardize
#'
#' Centers and scales to mean=0, sd=1.
#'
#' @param x Numeric vector.
#'
#' @return Numeric vector.
#'
#' @family data-transformation
#' @export
#'
#' @examples
#' iaw$standardize(c(1, 2, 3, 4, 5))

iaw$standardize <- function(x) {
    stopifnot(is.numeric(x))
    scale(x)[, 1]
}
