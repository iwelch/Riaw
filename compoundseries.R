#' Compound Returns Over Time
#'
#' @name compoundseries
#'
#' Calculates cumulative compounded returns.
#'
#' @param rate.of.return.timeseries Numeric vector of returns.
#' @param window Compounding window (0 = from start).
#' @param geomean If TRUE, return geometric mean.
#' @param na.is.zero If TRUE, treat NA as zero.
#'
#' @return Numeric vector of compounded returns.
#'
#' @family time-series
#' @export
#'
#' @examples
#' returns <- c(0.02, -0.01, 0.03, 0.01)
#' iaw$compoundseries(returns)

iaw$compoundseries <- function(rate.of.return.timeseries, window = 0,
                                geomean = FALSE, na.is.zero = FALSE) {
    stopifnot(is.numeric(rate.of.return.timeseries))
    stopifnot(is.numeric(window), length(window) == 1L)
    stopifnot(is.logical(geomean), length(geomean) == 1L)
    stopifnot(is.logical(na.is.zero), length(na.is.zero) == 1L)
    
    if (!na.is.zero) {
        stopifnot(!any(is.na(rate.of.return.timeseries)))
    }
    
    xx <- rate.of.return.timeseries
    if (na.is.zero) xx[is.na(xx)] <- 0
    
    if (window == 0) {
        logsum <- iaw$cumulateseries(log(1 + xx))
        if (geomean) return(exp(logsum / seq_along(logsum)) - 1)
        xx.w.0 <- exp(logsum) - 1
        if (na.is.zero) xx.w.0[is.na(rate.of.return.timeseries)] <- NA
        return(xx.w.0)
    }
    
    cx <- c(0, iaw$compoundseries(xx, 0))
    prevseries <- 1 + iaw$lagseries(cx, window)
    rv <- (1 + cx) / ifelse(is.na(prevseries), 1.0, prevseries) - 1
    rv <- rv[2:length(rv)]
    if (na.is.zero) rv[is.na(rate.of.return.timeseries)] <- NA
    rv
}
