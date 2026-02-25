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
#' iaw$compoundseries(returns)   # cumulative: 0.02, 0.0098, 0.0401, 0.0505
#'
#' # Rolling 3-period compound return
#' iaw$compoundseries(returns, window = 3)
#'
#' # Geometric mean return at each point
#' iaw$compoundseries(returns, geomean = TRUE)
#'
#' # NA treated as zero return (missing data gap)
#' ret_with_gap <- c(0.01, NA, 0.02, 0.03)
#' iaw$compoundseries(ret_with_gap, na.is.zero = TRUE)
#'
#' # Annual equity returns: cumulative growth of $1
#' annual <- c(0.12, -0.05, 0.20, 0.08, -0.10)
#' 1 + iaw$compoundseries(annual)  # wealth path starting at $1
#'
#' # Rolling 12-month return from monthly data
#' monthly <- c(0.01, -0.02, 0.03, 0.01, 0.02, -0.01,
#'              0.01, 0.03, -0.01, 0.02, 0.01, 0.02, 0.01, -0.01)
#' iaw$compoundseries(monthly, window = 12)
#'
#' # Geometric mean: annualized return at each point
#' iaw$compoundseries(c(0.10, 0.20, -0.05), geomean = TRUE)  # smoothed avg

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
    
    ## Rolling window compound return: (1+cumret[t]) / (1+cumret[t-window]) - 1
    ## Prepend 0 so the division works for the first `window` observations too.
    cx <- c(0, iaw$compoundseries(xx, 0))        # cumulative returns, length n+1
    prevseries <- 1 + iaw$lagseries(cx, window)   # denominator: wealth at t-window
    rv <- (1 + cx) / ifelse(is.na(prevseries), 1.0, prevseries) - 1
    rv <- rv[2:length(rv)]                         # strip the prepended 0
    if (na.is.zero) rv[is.na(rate.of.return.timeseries)] <- NA
    rv
}
