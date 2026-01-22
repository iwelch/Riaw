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
    if (na.rm) x <- x[!is.na(x)]
    n <- length(x)
    if (n == 0) return(NA_real_)
    var(x) * (n - 1) / n
}
