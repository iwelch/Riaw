#' Cumulative Sum of a Time Series
#'
#' @name cumulateseries
#'
#' Wrapper around cumsum with validation.
#'
#' @param seriesin Numeric vector.
#'
#' @return Numeric vector of cumulative sums.
#'
#' @family time-series
#' @export
#'
#' @examples
#' iaw$cumulateseries(c(1, 2, 3, 4, 5))

iaw$cumulateseries <- function(seriesin) {
    stopifnot(is.numeric(seriesin))
    stopifnot(length(seriesin) >= 1L)
    cumsum(seriesin)
}
