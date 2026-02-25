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
#'
#' # Population SD of a known distribution: {1, 2, 3, 4, 5}
#' iaw$sdp(1:5)           # sqrt(2) = 1.4142
#'
#' # Relationship to sample SD: sdp = sd * sqrt((n-1)/n)
#' x <- c(10, 20, 30, 40)
#' all.equal(iaw$sdp(x), sd(x) * sqrt(3/4))  # TRUE
#'
#' # Useful for computing realized volatility from full population of returns
#' daily_ret <- c(0.01, -0.02, 0.005, 0.015, -0.01)
#' iaw$sdp(daily_ret) * sqrt(252)  # annualized population vol

iaw$sdp <- function(x, na.rm = TRUE) {
    sqrt(iaw$varp(x, na.rm = na.rm))
}
