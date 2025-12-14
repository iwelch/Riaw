#' Population Standard Deviation
#'
#' @name sdp
#'
#' Calculates population SD (n denominator).
#'
#' @param x Numeric vector.
#' @param na.rm Remove NA values.
#'
#' @return Standard deviation value.
#'
#' @family statistics
#' @export
#'
#' @examples
#' iaw$sdp(1:10)

iaw$sdp <- function(x, na.rm = TRUE) {
    sqrt(iaw$varp(x, na.rm = na.rm))
}
