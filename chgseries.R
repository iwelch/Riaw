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
#'
#' # Daily P&L changes from a cumulative equity curve
#' equity <- c(1000, 1015, 1008, 1025, 1040)
#' iaw$chgseries(equity)   # NA  15  -7  17  15
#'
#' # Compute quarterly GDP growth (change from previous quarter)
#' gdp <- c(21000, 21200, 21150, 21400, 21600)
#' iaw$chgseries(gdp)   # NA 200 -50 250 200
#'
#' # Year-over-year change with numlags = 4 (quarterly data)
#' quarterly <- c(100, 102, 105, 108, 112, 115, 120, 125)
#' iaw$chgseries(quarterly, numlags = 4)  # first 4 are NA, then YoY diffs

iaw$chgseries <- function(seriesin, numlags = 1, panelid = NULL, timeid = NULL) {
    stopifnot(is.numeric(seriesin))
    stopifnot(length(seriesin) > 1L)
    seriesin - iaw$lagseries(seriesin, numlags, panelid, timeid)
}

#' @rdname chgseries
#' @export
iaw$dchgseries <- iaw$chgseries
