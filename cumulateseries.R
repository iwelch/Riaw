
#' CUMULATESERIES
#'
#' @name cumulateseries
#'
#'  shortcut for cumsum
#'
#' @usage cumulate a series
#'
#' @param seriesin a time series
#'
#' @return
#'
#' @seealso cumsum
#'


iaw$cumulateseries <- function (seriesin) {
    (is.numeric(seriesin)) %or% "Looks like your series is not a vector in cumulateseries but {{class(seriesin)}}"
    (length(seriesin) >= 1) %or% "Need more observations than {{length(seriesin)}} (<=1)!"
    cumsum(seriesin)
}
