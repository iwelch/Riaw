#' Population Standard Deviation
#'
#' Calculates the population standard deviation (divides by n, not n-1).
#'
#' @param x A numeric vector, matrix, or data frame.
#' @param ... Additional arguments passed to \code{var()}.
#'
#' @return The population standard deviation.
#'
#' @details
#' R's \code{sd()} function computes sample standard deviation.
#' This function adjusts to population standard deviation.
#'
#' @export
#'
#' @seealso \code{\link{sd}}, \code{\link{iaw$varp}}, \code{\link{iaw$covp}}
#'
#' @examples
#' x <- rnorm(100)
#'
#' sd(x)        # Sample SD (n-1)
#' iaw$sdp(x)   # Population SD (n)

iaw$sdp <- function(x, ...) {
    sqrt(iaw$varp(x, ...))
}
