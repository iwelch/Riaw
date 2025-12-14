#' Lead-Lag Partial Cross-Correlations of Two Time Series
#'
#' Calculates partial cross-correlations between two time series at various
#' leads and lags using multiple regression. Unlike simple correlations, partial
#' correlations control for other lags.
#'
#' @param series.x First numeric vector (time series).
#' @param series.y Second numeric vector (time series), same length as series.x.
#' @param around Integer specifying how many leads/lags to include (default 5).
#'
#' @return A named numeric vector of partial correlation coefficients (regression
#'   coefficients on standardized variables). Names indicate the lag:
#'   \code{pcor-5} through \code{pcor5}.
#'
#' @details
#' The function standardizes both series, creates a matrix of lagged versions of
#' series.x, and regresses series.y on all lags simultaneously. The coefficients
#' represent partial correlations controlling for other lags.
#'
#' @export
#'
#' @seealso \code{\link{iaw$autocorrel}} for simple correlations, \code{\link{pacf}}
#'
#' @examples
#' # Compare simple and partial correlations
#' x <- sin(1:50) + rnorm(50, sd = 0.1)
#' y <- sin(2:51) + rnorm(50, sd = 0.1)
#'
#' # Simple correlations
#' iaw$autocorrel(x, y, around = 3)
#'
#' # Partial correlations (controlling for other lags)
#' iaw$autopcorrel(x, y, around = 3)

iaw$autopcorrel <- function(series.x, series.y, around = 5) {
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

    m <- matrix(nrow = length(series.x), ncol = (around * 2 + 1))
    for (i in -around:+around) {
        m[, i + around + 1] <- scale(iaw$lagseries(series.x, i))
    }
    series.y <- scale(series.y)

    robj <- lm(series.y ~ m)
    coefs <- coefficients(robj)

    for (i in -around:+around) {
        names(coefs)[i + around + 2] <- paste0("pcor", i)
    }

    coefs
}
