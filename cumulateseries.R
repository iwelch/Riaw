#' Cumulative Sum of a Time Series
#'
#' @name cumulateseries
#'
#' Wrapper around cumsum with validation.
#'
#' @param seriesin Numeric vector.
#'
#' @return Numeric vector of cumulative sums.
#'
#' @family time-series
#' @export
#'
#' @examples
#' # Simple cumulative sum
#' iaw$cumulateseries(c(1, 2, 3, 4, 5))  # 1 3 6 10 15
#'
#' # Cumulative returns: start at 0, accumulate daily P&L
#' daily_pnl <- c(0.01, -0.005, 0.02, -0.01, 0.015)
#' iaw$cumulateseries(daily_pnl)
#'
#' # Negative steps produce a declining series
#' iaw$cumulateseries(c(10, -3, -2, -1, 5))

iaw$cumulateseries <- function(seriesin) {
    stopifnot(is.numeric(seriesin))
    stopifnot(length(seriesin) >= 1L)
    cumsum(seriesin)
}
