#' Population Covariance
#'
#' Calculates the population covariance (divides by n, not n-1).
#'
#' @param x First numeric vector or matrix.
#' @param y Second numeric vector or matrix.
#' @param ... Additional arguments passed to \code{cov()}.
#'
#' @return The population covariance.
#'
#' @details
#' R's \code{cov()} function computes sample covariance (divides by n-1).
#' This function adjusts to population covariance (divides by n).
#'
#' @export
#'
#' @seealso \code{\link{cov}}, \code{\link{iaw$varp}}, \code{\link{iaw$sdp}}
#'
#' @examples
#' x <- rnorm(100)
#' y <- x + rnorm(100)
#'
#' cov(x, y)        # Sample covariance (n-1)
#' iaw$covp(x, y)   # Population covariance (n)

iaw$covp <- function(x, y, ...) {
    lx <- if (is.data.frame(x) | is.matrix(x)) nrow(x) else length(x)
    ly <- if (is.data.frame(y) | is.matrix(y)) nrow(y) else length(y)
    (lx == ly) %or% "x and y must have the same number of observations"
    (lx - 1) / lx * cov(x, y, ...)
}
