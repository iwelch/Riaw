
#' PCHGSERIES
#'
#' @name pchgseries
#'
#'   the percent change of a time series (often the rate of return)
#'
#' @usage pchgseries (seriesin, numlags = 1, panelid = NULL, timeid = NULL)
#'
#' @param seriesin a time series vector
#'
#' @return a series of equal length
#'


iaw$pchgseries <- function (seriesin, numlags = 1, panelid = NULL, timeid=NULL) {
  (is.numeric(seriesin)) %or% "Looks like your series is not a vector in pchgseries"
  (length(seriesin) > 1) %or% "Need more observations than {{length(seriesin)}} (<=1)!"
  seriesin/iaw$lagseries(seriesin, numlags, panelid, timeid) - 1
}
