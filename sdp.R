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
#' iaw$sdp(1:10)          # population SD ~2.872 (vs sd() ~3.028)
#'
#' # Compare with sample SD: sdp is always smaller
#' x <- c(2, 4, 4, 4, 5, 5, 7, 9)
#' iaw$sdp(x)             # 2 (exact population SD of this population)
#' sd(x)                  # 2.138 (sample SD with n-1)
#'
#' # NA values are removed by default
#' iaw$sdp(c(1, 2, NA, 4, 5))
#'
#' # Constant vector: SD is 0
#' iaw$sdp(rep(5, 10))    # 0

iaw$sdp <- function(x, na.rm = TRUE) {
    sqrt(iaw$varp(x, na.rm = na.rm))
}
