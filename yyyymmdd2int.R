
#' YYYYMMDD.TOGGLE
#'
#' @name yyyymmdd.toggle
#'
#' converts a yyyymmdd date to an integer date or vice-versa
#'
#' @usage iaw$yyyymmdd.toggle(20250101)
#'
#' @param yyyymmdd_or_int
#'
#' @return the opposite
#'

iaw$yyyymmdd.toggle <- function( yyyymmdd_or_int ) {
    if (all( is.na(yyyymmdd_or_int) )) return(yyyymmdd_or_int)
    stopifnot( is.numeric(yyyymmdd_or_int) )

    ## 1970-01-01 is day 0

    if ( all( is.na(yyyymmdd_or_int) | (yyyymmdd_or_int >= 19000000) ) )
        return( as.numeric( as.Date( as.character(yyyymmdd_or_int), format = '%Y%m%d') ) )

    if ( all( is.na(yyyymmdd_or_int) | ((yyyymmdd_or_int <= 50000)&(yyyymmdd_or_int >= -300000)) ))
        return( as.numeric(format(as.Date(yyyymmdd_or_int), "%Y%m%d")) )

    stop("Unrecognized Elements in yyyymmdd.toggle")

}

################################################################


#' YYYYMMDD
#'
#' @name yyyymmdd
#'
#' converts any dateI can imagine into a yyyymmdd date
#'
#' @usage iaw$yyyymmdd <- function( anything, output=c("posix", "gregorian", "yyyymmdd", "weeknum", "weekday") )
#'
#' @param anything
#'
#' @param output
#'
#' @seealso as.PlotDate.yyyymmdd
#'
#' @return yyyymmdd
#'


iaw$yyyymmdd <- function( anything, output=c("posix", "gregorian", "yyyymmdd", "weeknum", "weekday") ) {

    ## this whole function should be rewritten.
    ## the intent is to get back a date in yyyymmdd format regardless of whether the format is
    ##   posix: 2024-12-31 (or YYYY-MM-DD HH:MM:SS)
    ##   gregorian: 1735603200 (divide by 86,400=secs/day to get it from Jan 1, 1970)
    ##   yyyymmdd: pass back

    if (length(output)!=1) {
        message("Please give desired output format, one of ")
        print(output)
        stop("please choose output format in call")
    }

    if (all( is.na(anything) )) return(anything)

    iaw$is.POSIXct <- function(x) inherits(x, "POSIXct")

    ## first convert other formats, like yyyymmdd or 10000 (=19970519) into a string 

    if ( ! iaw$is.POSIXct( anything ) ) {
        stopifnot( is.numeric( anything ) )
        if ( all( is.na(anything) | (anything >= 19000000) ) ) {
            anything <- as.Date( as.character(anything), format = '%Y%m%d')
        } else if ( all( is.na(anything) | ((anything <= 50000)&(anything >= -100000)) )) {
            anything <- as.Date(anything, "%Y%m%d")
        }
    }

    if (output == "posix") {
        return(anything)
    } else if (output %in% c("weeknum","wnum")) {
        return(as.integer( (as.numeric(anything)-4)/7 ))
    } else if (output %in% c("weekday","wday")) {
        return(format(anything, format="%a"))
    }
    stop(paste0("don't know desired output format ",output))
}

## test : stopifnot( iaw$yyyymmdd( c(20240602, 20240603), output="wnum") == c(2838,2839) ) ## week begins on Monday
