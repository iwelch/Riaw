#' Calculate Percent Changes in a Time Series
#'
#' @name pchgseries
#'
#' Computes percent change: x[t]/x[t-n] - 1.
#'
#' @param seriesin Numeric vector.
#' @param numlags Number of periods. Default 1.
#' @param panelid Optional panel identifier.
#' @param timeid Optional time identifier.
#'
#' @return Numeric vector of same length.
#'
#' @family time-series
#' @export
#'
#' @examples
#' prices <- c(100, 105, 103, 110)
#' iaw$pchgseries(prices)

iaw$pchgseries <- function(seriesin, numlags = 1, panelid = NULL, timeid = NULL) {
    stopifnot(is.numeric(seriesin))
    stopifnot(length(seriesin) > 1L)
    seriesin / iaw$lagseries(seriesin, numlags, panelid, timeid) - 1
}
