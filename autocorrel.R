#' Lead-Lag Cross-Correlation of Two Time Series
#'
#' Calculates cross-correlations between two time series at various leads and
#' lags. Useful for identifying lead-lag relationships and optimal timing offsets.
#'
#' @param series.x First numeric vector (time series).
#' @param series.y Second numeric vector (time series), same length as series.x.
#' @param around Integer specifying how many leads/lags to compute (default 5).
#'   Computes correlations from \code{-around} to \code{+around}.
#'
#' @return A named numeric vector of correlations. Names indicate the lag:
#'   \code{cor-5} through \code{cor5} for \code{around=5}.
#'
#' @details
#' Positive lags mean series.x is lagged (shifted back) relative to series.y.
#' The correlation at lag k is: \code{cor(x[t-k], y[t])}.
#'
#' @export
#'
#' @seealso \code{\link{iaw$autopcorrel}} for partial correlations,
#'   \code{\link{ccf}}, \code{\link{acf}}
#'
#' @examples
#' # Create two related series
#' x <- sin(1:100)
#' y <- sin(2:101)  # y leads x by 1 period
#'
#' iaw$autocorrel(x, y, around = 3)
#' # Highest correlation should be at cor1 (x lagged 1 matches y)
#'
#' # Check if stock returns predict future volume
#' # returns <- diff(log(prices))
#' # volume <- trading_volume
#' # iaw$autocorrel(returns, volume, around = 5)

autocorrel <- function(series.x, series.y, around = 5) {
    if (!is.null(getOption("strict"))) {
        (is.null(series.x)) %and% "series.x is null"
        (is.vector(series.x, mode = "numeric")) %or%
            "series.x must be numeric vector, not {{class(series.x)}}"
        (length(series.x) > 1) %or% "series.x needs more observations"

        (is.null(series.y)) %and% "series.y is null"
        (is.vector(series.y, mode = "numeric")) %or%
            "series.y must be numeric vector, not {{class(series.y)}}"
        (length(series.y) > 1) %or% "series.y needs more observations"

        (iaw$is.numeric(around, 1)) %or% "around must be a single integer"
    }

    v <- numeric(2 * around + 1)
    for (i in -around:+around) {
        v[i + around + 1] <- cor(iaw$lagseries(series.x, i), series.y, use = "pair")
        names(v)[i + around + 1] <- paste0("cor", i)
    }

    v
}

#' @export
iaw$autocorrel <- autocorrel
