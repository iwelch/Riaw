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
#'
#' # With a macro series: does GDP growth predict future inflation?
#' set.seed(42)
#' gdp <- cumsum(rnorm(80))
#' inf <- 0.4 * c(NA, gdp[-80]) + rnorm(80)   # inflation lags GDP by 1
#' pc <- iaw$autopcorrel(gdp, inf, around = 4)
#' pc   # pcor-1 should be largest, showing GDP leads inflation by 1 period
#'
#' # Compare partial correlations to simple cross-correlations
#' cc <- iaw$autocorrel(gdp, inf, leadlags = 4)
#' rbind(partial = pc[-1], simple = cc)  # partial controls for other lags
#'
#' # Partial autocorrelation of an AR(1) process (self vs self)
#' set.seed(10)
#' ar1 <- as.numeric(filter(rnorm(150), 0.6, method = "recursive"))
#' pac <- iaw$autopcorrel(ar1, ar1, around = 3)
#' pac   # pcor0 dominates; higher-order partial correlations near zero
#'
#' # Volume vs returns: isolate the direct lag-0 relationship
#' set.seed(55)
#' ret <- rnorm(100, sd = 0.02)
#' vol <- 1e6 + 3e7 * abs(ret) + rnorm(100, sd = 5e3)
#' iaw$autopcorrel(abs(ret), vol, around = 2)  # pcor0 strongest

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
    
    names(coefs)[1] <- "(Intercept)"  ## index 1 is the intercept
    for (i in -around:+around) {
        names(coefs)[i + around + 2] <- paste0("pcor", i)  ## indices 2.. are the lag coefficients
    }
    coefs
}
