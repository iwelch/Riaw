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
#' iaw$chgseries(x)   # first difference: NA, 2, 3, -2, 5
#'
#' # 2-period difference (change from two periods ago)
#' iaw$chgseries(x, numlags = 2)
#'
#' # Panel data: differences respect group boundaries via panelid
#' prices <- c(100, 102, 105, 200, 198, 203)
#' firm   <- c("A",  "A",  "A", "B",  "B",  "B")
#' iaw$chgseries(prices, panelid = firm)  # no cross-firm differencing

iaw$chgseries <- function(seriesin, numlags = 1, panelid = NULL, timeid = NULL) {
    stopifnot(is.numeric(seriesin))
    stopifnot(length(seriesin) > 1L)
    seriesin - iaw$lagseries(seriesin, numlags, panelid, timeid)
}

#' @rdname chgseries
#' @export
iaw$dchgseries <- iaw$chgseries
