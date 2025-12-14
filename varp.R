#' Population Variance
#'
#' @name varp
#'
#' Calculates population variance (n denominator).
#'
#' @param x Numeric vector.
#' @param na.rm Remove NA values.
#'
#' @return Variance value.
#'
#' @family statistics
#' @export
#'
#' @examples
#' iaw$varp(1:10)

iaw$varp <- function(x, na.rm = TRUE) {
    stopifnot(is.numeric(x))
    n <- length(x)
    var(x, na.rm = na.rm) * (n - 1) / n
}
