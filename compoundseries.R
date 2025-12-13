
#' COMPOUNDSERIES
#'
#' @name compoundseries
#'
#' compound the rate of returns and return the series.  if series has NA, na.is.zero must be set to true
#'
#' @usage compoundseries(rate.of.return.timeseries)
#'
#' @param 0 means from start to whenever
#'
#' @return equal length vector
#'
#' @seealso cmpsum
#'


iaw$compoundseries <- function( rate.of.return.timeseries, window=0, geomean=FALSE, na.is.zero = FALSE ) {

    ## cumprod( (1 + rate.of.return.timeseries) ) -1

    (is.numeric(rate.of.return.timeseries)) %or% "compoundseries: rate of return series must be numeric"
    (iaw$is.scalar(window)) %or% "compoundseries: window length must be a scalar"
    (is.numeric(window)) %or% "compoundseries: window length must be numeric"
    (iaw$is.scalar(geomean)) %or% "compoundseries: geomean must be a scalar, not {{iaw$whatis(geomean)}}"
    (is.logical(geomean)) %or% "compoundseries: geomean must be logical"
    (iaw$is.scalar(na.is.zero)) %or% "compoundseries: na.is.zero must be a scalar"
    (is.logical(na.is.zero)) %or% "compoundseries: geomean must be logical"

    if (!na.is.zero) {
        all( !is.na(rate.of.return.timeseries ) ) %or% "compoundseries: ERROR: if na's are in the series, this function does not work"
    }

    xx <- rate.of.return.timeseries
    if (na.is.zero) xx[is.na(xx)] <- 0

    if (window==0) {
        logsum <- iaw$cumulateseries(log(1 + xx))
        if (geomean) return( exp( logsum/length(logsum) ) -1 )

        xx.w.0 <- exp( logsum ) - 1
        if (na.is.zero) xx.w.0[ is.na(xx) ] <- NA  ## whatever was NA should remain NA
        return(xx.w.0)
    }

    cx <- c(0, iaw$compoundseries(xx,0))
    prevseries <- (1+iaw$lagseries(cx,window))

    rv <- (1+cx)/ifelse( is.na(prevseries), 1.0, prevseries ) - 1  # insufficient fix
    rv <- rv[2:length(rv)]

    if (na.is.zero) rv[ is.na(xx) ] <- NA  # special case.  if r is NA, so is compound backwards

    rv
}
