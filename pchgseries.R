#' Calculate Percent Changes (Returns) in a Time Series
#'
#' Computes the percent change (rate of return) of a time series:
#' \code{x[t]/x[t-n] - 1}. For panel data, returns do not cross panel boundaries.
#'
#' @param seriesin A numeric vector (time series), typically prices or levels.
#' @param numlags Integer number of periods for the return calculation. Default is 1.
#' @param panelid Optional vector of panel identifiers (e.g., firm IDs).
#' @param timeid Optional vector of time identifiers (required with panelid).
#'
#' @return A numeric vector of the same length as \code{seriesin}, with NA
#'   values for the first \code{numlags} observations (or at panel boundaries).
#'
#' @export
#'
#' @seealso \code{\link{iaw$chgseries}} for absolute changes,
#'   \code{\link{iaw$compoundseries}} for cumulative returns
#'
#' @examples
#' # Simple returns from prices
#' prices <- c(100, 105, 103, 110, 115)
#' iaw$pchgseries(prices)
#' # NA 0.05 -0.019 0.068 0.045
#'
#' # Monthly returns to annual returns (12-month return)
#' iaw$pchgseries(prices, numlags = 2)
#'
#' # Panel data
#' d <- data.frame(
#'     stock = c("AAPL", "AAPL", "AAPL", "GOOG", "GOOG", "GOOG"),
#'     date = rep(1:3, 2),
#'     price = c(100, 105, 110, 200, 210, 220)
#' )
#' d$ret <- iaw$pchgseries(d$price, panelid = d$stock, timeid = d$date)
#' d

iaw$pchgseries <- function(seriesin, numlags = 1, panelid = NULL, timeid = NULL) {
    (is.numeric(seriesin)) %or% "seriesin must be numeric, not {{class(seriesin)}}"
    (length(seriesin) > 1) %or% "Need more than {{length(seriesin)}} observations"
    seriesin / iaw$lagseries(seriesin, numlags, panelid, timeid) - 1
}
