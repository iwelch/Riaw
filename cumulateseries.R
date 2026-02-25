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
#'
#' # Cumulative log-returns for compounding via exp()
#' log_ret <- log(1 + c(0.01, 0.02, -0.005, 0.015))
#' exp(iaw$cumulateseries(log_ret)) - 1  # cumulative simple returns
#'
#' # Running total of trade volumes throughout the day
#' volumes <- c(1500, 3200, 2800, 4100, 1900)
#' iaw$cumulateseries(volumes)  # 1500 4700 7500 11600 13500
#'
#' # Single element: returns itself unchanged
#' iaw$cumulateseries(42)  # 42

iaw$cumulateseries <- function(seriesin) {
    stopifnot(is.numeric(seriesin))
    stopifnot(length(seriesin) >= 1L)
    cumsum(seriesin)
}
