#' Moving Average
#'
#' @name ma
#'
#' Calculates moving average.
#'
#' @param x Numeric vector.
#' @param n Window size.
#'
#' @return Numeric vector of moving averages.
#'
#' @family time-series
#' @export
#'
#' @examples
#' iaw$ma(1:10, 3)

iaw$ma <- function(x, n) {
    stopifnot(is.numeric(x))
    stopifnot(is.numeric(n), length(n) == 1L, n >= 1)
    
    stats::filter(x, rep(1/n, n), sides = 1)
}
