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
    n <- length(x)
    cov(x, y, use = if (na.rm) "complete.obs" else "everything") * (n - 1) / n
}
