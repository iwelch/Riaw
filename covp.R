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
#' # Population covariance (n denominator) vs sample covariance (n-1)
#' x <- 1:10
#' iaw$covp(x, x)   # population variance of x with itself
#' cov(x, x)        # sample covariance (slightly larger)
#'
#' # Two related series
#' iaw$covp(c(1, 2, 3, 4, 5), c(2, 4, 5, 4, 5))
#'
#' # NA values are removed pairwise by default
#' iaw$covp(c(1, NA, 3, 4), c(2, 3, NA, 5))
#'
#' # All columns of a matrix against a reference vector
#' m <- matrix(c(1:5, 5:1, c(1,3,5,3,1)), ncol = 3)
#' apply(m, 2, function(col) iaw$covp(col, 1:5))

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
    if (n == 1) return(0)
    cov(x, y) * (n - 1) / n
}
