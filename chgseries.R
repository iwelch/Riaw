#' Calculate Changes in a Time Series
#'
#' @name chgseries
#'
#' Computes first difference: x[t] - x[t-n].
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
#' x <- c(100, 102, 105, 103, 108)
#' iaw$chgseries(x)

iaw$chgseries <- function(seriesin, numlags = 1, panelid = NULL, timeid = NULL) {
    stopifnot(is.numeric(seriesin))
    stopifnot(length(seriesin) > 1L)
    seriesin - iaw$lagseries(seriesin, numlags, panelid, timeid)
}

iaw$dchgseries <- iaw$chgseries
