
#' CHGSERIES
#'
#' @name chgseries
#'
#' calculate the element to element changes in a time-series
#'
#' @usage chgseries (seriesin, numlags = 1, panelid = NULL, timeid=NULL)
#'
#' @param seriesin a time series
#' @param numlags the number of lags, typically 1
#' @param panelid for panel use
#'
#' @return
#'
#' @aliases dchgseries


iaw$chgseries <- function (seriesin, numlags = 1, panelid = NULL, timeid =NULL) {
    (is.numeric(seriesin)) %or% "Looks like your series is not a vector in chgseries but a {{class(seriesin)}}"
    (length(seriesin) > 1) %or% "Need more observations than {{length(seriesin)}} (<=1)!"
    seriesin - iaw$lagseries(seriesin, numlags, panelid, timeid)
}

iaw$dchgseries <- iaw$chgseries
