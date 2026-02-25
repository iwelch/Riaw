#' Cross-Correlation at Various Lags
#'
#' @name autocorrel
#'
#' Calculates cross-correlations at different leads/lags.
#'
#' **WARNING**: Be careful that you understand what the lead and what the lag is
#'
#' @param series.x First numeric vector.
#' @param series.y Second numeric vector.
#' @param leadlags Number of leads/lags (default 5).
#'
#' @return Named numeric vector of correlations.
#'
#' @family statistics
#' @export
#'
#' @examples
#' x <- sin(1:100)
#' y <- sin(2:101)
#' iaw$autocorrel(x, y, leadlags = 3)
#'
#' # Stock returns: does yesterday's return predict today's?
#' set.seed(7)
#' ret <- rnorm(200)
#' iaw$autocorrel(ret, ret, leadlags = 5)   # near-zero: consistent with EMH
#'
#' # Macro: CPI changes vs industrial production with asymmetric lags
#' cpi <- cumsum(rnorm(120, mean = 0.002))
#' ip  <- 0.5 * c(rep(NA, 3), cpi[-(120:118)]) + rnorm(120)
#' iaw$autocorrel(ip, cpi, leadlags = 5)   # positive cor3: IP leads CPI by 3

iaw$autocorrel <- function(series.x, series.y, leadlags = 5) {
    stopifnot(is.numeric(series.x), is.vector(series.x))
    stopifnot(is.numeric(series.y), is.vector(series.y))
    stopifnot(length(series.x) > 1L, length(series.y) > 1L)
    stopifnot(is.numeric(leadlags), length(leadlags) == 1L)

    v <- numeric(2 * leadlags + 1)
    for (i in -leadlags:+leadlags) {
        v[i + leadlags + 1] <- cor(iaw$lagseries(series.x, i), series.y, use = "pair")
        names(v)[i + leadlags + 1] <- paste0("cor", i)
    }
    v
}
