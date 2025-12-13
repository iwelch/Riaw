
#' Lead-Lag Autocorrelation of Two Time-Series Vectors
#'
#' calculate a vector of cross-correlations between two time-series at different leads and lags.
#'
#' @name autocorrel
#'
#' @param series.x  the first timeseries
#'
#' @param  series.y  the second timeseries
#'
#' @return a vector of autocorrelation coefficients
#'
#' @examples
#'   > iaw$autocorrel( sin(1:9), sin(0:8) )
#'       cor-5     cor-4     cor-3     cor-2     cor-1      cor0      cor1      cor2      cor3      cor4      cor5
#'    0.884559  0.001787 -0.682950 -0.987981 -0.328011  0.542402  1.000000  0.603139 -0.458924 -0.994482 -0.886689
#'
#' @seealso autopcorrel
#'

autocorrel <- function (series.x, series.y, around = 5) {
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

  v <- NULL
  for (i in -around:+around)
    v[i + around + 1] <- cor(iaw$lagseries(series.x, i), series.y, use = "pair")
  for (i in -around:+around) {
    names(v)[i + around + 1] <- paste0("cor", i)
  }
  return(v)
}

iaw$autocorrel <- autocorrel
