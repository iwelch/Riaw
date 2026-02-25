#' Moving Average
#'
#' @name ma
#'
#' Calculates moving average.
#'
#' @param x Numeric vector.
#' @param n Window size.
#' @param around Logical. If \code{TRUE}, center-aligned; if \code{FALSE}
#'   (default), right-aligned.
#'
#' @return Numeric vector of moving averages.
#'
#' @family time-series
#' @export
#'
#' @examples
#' iaw$ma(1:10, 3)
#'
#' # Right-aligned 5-day moving average of noisy prices
#' prices <- c(100, 102, 101, 105, 103, 107, 110, 108, 112, 115)
#' iaw$ma(prices, 5)   # first 4 values are NA (right-aligned)
#'
#' # Centered (symmetric) moving average smooths a noisy signal
#' x <- sin(seq(0, 2 * pi, length.out = 50)) + rnorm(50, sd = 0.2)
#' smooth <- iaw$ma(x, 7, around = TRUE)
#' cor(smooth, sin(seq(0, 2 * pi, length.out = 50)), use = "complete.obs")

iaw$ma <- function(x, n, around = FALSE) {
    stopifnot(is.numeric(x))
    stopifnot(is.numeric(n), length(n) == 1L, n >= 1)

    zoo::rollmean(x, n, fill = NA, align = if (around) "center" else "right")
}
