#' Population Variance
#'
#' Calculates the population variance (divides by n, not n-1).
#'
#' @param x A numeric vector, matrix, or data frame.
#' @param ... Additional arguments passed to \code{var()}.
#'
#' @return The population variance.
#'
#' @details
#' R's \code{var()} function computes sample variance (divides by n-1).
#' This function adjusts to population variance (divides by n).
#'
#' @export
#'
#' @seealso \code{\link{var}}, \code{\link{iaw$sdp}}, \code{\link{iaw$covp}}
#'
#' @examples
#' x <- rnorm(100)
#'
#' var(x)        # Sample variance (n-1)
#' iaw$varp(x)   # Population variance (n)

iaw$varp <- function(x, ...) {
    lx <- if (is.data.frame(x) | is.matrix(x)) nrow(x) else length(x)
    (lx - 1) / lx * var(x, ...)
}
