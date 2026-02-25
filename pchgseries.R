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
#' iaw$pchgseries(prices)    # NA, 0.05, -0.019, 0.068
#'
#' # Two-period change (quarter-over-quarter from monthly data)
#' monthly <- c(100, 102, 101, 105, 104, 108)
#' iaw$pchgseries(monthly, numlags = 2)
#'
#' # Negative prices or zero cause Inf/NaN naturally; check for zeros
#' iaw$pchgseries(c(0, 5, 10))   # Inf at position 2 (divide by 0)
#'
#' # Confirm first element is always NA (no prior period)
#' x <- c(50, 60, 45)
#' ret <- iaw$pchgseries(x)
#' is.na(ret[1])   # TRUE
#'
#' # Compute daily returns from a stock price series
#' closing <- c(150.00, 152.25, 151.80, 154.10, 153.50)
#' daily_ret <- iaw$pchgseries(closing)
#' round(daily_ret, 4)  # NA, 0.0150, -0.0030, 0.0151, -0.0039
#'
#' # Year-over-year growth from quarterly GDP (4-period lag)
#' gdp <- c(100, 101, 103, 102, 105, 107, 110, 108)
#' yoy <- iaw$pchgseries(gdp, numlags = 4)
#' round(yoy, 3)  # NA NA NA NA 0.050 0.059 0.068 0.059
#'
#' # Log-returns vs simple returns (close approximation for small changes)
#' p <- c(100, 102, 99, 105)
#' simple <- iaw$pchgseries(p)
#' logreturns <- diff(log(p))
#' round(simple[-1] - logreturns, 6)  # differences are tiny

iaw$pchgseries <- function(seriesin, numlags = 1, panelid = NULL, timeid = NULL) {
    stopifnot(is.numeric(seriesin))
    stopifnot(length(seriesin) > 1L)
    seriesin / iaw$lagseries(seriesin, numlags, panelid, timeid) - 1
}
