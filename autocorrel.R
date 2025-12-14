#' Cross-Correlation at Various Lags
#'
#' @name autocorrel
#'
#' Calculates cross-correlations at different leads/lags.
#'
#' @param series.x First numeric vector.
#' @param series.y Second numeric vector.
#' @param around Number of leads/lags (default 5).
#'
#' @return Named numeric vector of correlations.
#'
#' @family statistics
#' @export
#'
#' @examples
#' x <- sin(1:100)
#' y <- sin(2:101)
#' iaw$autocorrel(x, y, around = 3)

autocorrel <- function(series.x, series.y, around = 5) {
    stopifnot(is.numeric(series.x), is.vector(series.x))
    stopifnot(is.numeric(series.y), is.vector(series.y))
    stopifnot(length(series.x) > 1L, length(series.y) > 1L)
    stopifnot(is.numeric(around), length(around) == 1L)
    
    v <- numeric(2 * around + 1)
    for (i in -around:+around) {
        v[i + around + 1] <- cor(iaw$lagseries(series.x, i), series.y, use = "pair")
        names(v)[i + around + 1] <- paste0("cor", i)
    }
    v
}

iaw$autocorrel <- autocorrel
