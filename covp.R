#' Population Covariance
#'
#' @name covp
#'
#' Calculates population covariance (n denominator).
#'
#' @param x First vector.
#' @param y Second vector.
#' @param na.rm Remove NA values.
#'
#' @return Covariance value.
#'
#' @family statistics
#' @export
#'
#' @examples
#' iaw$covp(1:10, 11:20)

iaw$covp <- function(x, y, na.rm = TRUE) {
    stopifnot(is.numeric(x), is.numeric(y))
    stopifnot(length(x) == length(y))
    if (na.rm) {
        complete <- !is.na(x) & !is.na(y)
        x <- x[complete]
        y <- y[complete]
    }
    n <- length(x)
    if (n == 0) return(NA_real_)
    cov(x, y) * (n - 1) / n
}
