
#' Lead-Lag Partial Autocorrelations of Two Time-Series Vectors
#'
#' @name autopcorrel
#'
#' calculate a vector of partial cross-correlation between two time-series at different leads and lags, scaled.
#'
#' @param series.x the first timeseries
#' @param series.y the second timeseries
#'
#' @return a vector of partial autocorrelation coefficients
#'
#' @examples
#'  > iaw$autopcorrel( sin(1:30)+tan(1:30)/1000, sin(0:29), around=3 )
#'    (Intercept)      pcor-3      pcor-2      pcor-1       pcor0       pcor1       pcor2       pcor3
#'       0.008496   -0.191502   -0.317053   -0.170215    0.128599    0.295066    0.179672   -0.112650
#'
#' @export

iaw$autopcorrel <- function (series.x, series.y, around = 5) {
  if (!is.null(getOption("strict"))) {
    (is.null(series.x)) %and% "series.x is null"
    (is.vector(series.x, mode="numeric")) %or% "Your series is not a numeric vector, but a {{class(series.x)}}."
    (length(series.x) > 1) %or% "Need more observations than in series.x"

    (is.null(series.y)) %and% "series.y is null"
    (is.vector(series.y, mode="numeric")) %or% "Your series is not a numeric vector, but a {{class(series.y)}}."
    (length(series.y) > 1) %or% "Need more observations than in series.y"

    (is.vector(around, mode="numeric")) %or% "around must be a single integer"
    (iaw$is.numeric(around, 1)) %or% "around must be a single integer"
  }

  m <- matrix(nrow = length(series.x), ncol = (around * 2 + 1))
  for (i in -around:+around)
    m[, i + around + 1] <- scale(iaw$lagseries(series.x, i))
  series.y <- scale(series.y)
  robj <- lm(series.y ~ m)
  c <- coefficients(robj)
  for (i in -around:+around) {
    names(c)[i + around + 2] <- paste0("pcor", i)
  }
  return(c)
}
