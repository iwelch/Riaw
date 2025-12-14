#' Partial Cross-Correlation at Various Lags
#'
#' @name autopcorrel
#'
#' Calculates partial cross-correlations using regression.
#'
#' @param series.x First numeric vector.
#' @param series.y Second numeric vector.
#' @param around Number of leads/lags (default 5).
#'
#' @return Named numeric vector of partial correlations.
#'
#' @family statistics
#' @export
#'
#' @examples
#' x <- sin(1:50) + rnorm(50, sd = 0.1)
#' y <- sin(2:51) + rnorm(50, sd = 0.1)
#' iaw$autopcorrel(x, y, around = 3)

iaw$autopcorrel <- function(series.x, series.y, around = 5) {
    stopifnot(is.numeric(series.x), is.vector(series.x))
    stopifnot(is.numeric(series.y), is.vector(series.y))
    stopifnot(length(series.x) > 1L, length(series.y) > 1L)
    stopifnot(is.numeric(around), length(around) == 1L)
    
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
