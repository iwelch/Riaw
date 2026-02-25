#' Toggle Between YYYYMMDD Integer and Gregorian Day Number
#'
#' @name yyyymmdd.toggle
#'
#' Converts YYYYMMDD integers to Gregorian day numbers (days since 1970-01-01)
#' and vice versa. Autodetects direction by value range.
#'
#' @param yyyymmdd_or_int Numeric vector of YYYYMMDD integers (>= 19000000) or
#'   Gregorian day numbers (-300000 to 50000).
#'
#' @return Numeric vector converted to the other format.
#'
#' @family datetime
#' @export
#'
#' @examples
#' iaw$yyyymmdd.toggle(20210115)   # -> 18642 (days since 1970-01-01)
#' iaw$yyyymmdd.toggle(18642)      # -> 20210115 (back to YYYYMMDD)
#'
#' # Useful for doing date arithmetic: add 30 days then convert back
#' greg <- iaw$yyyymmdd.toggle(20230601)
#' iaw$yyyymmdd.toggle(greg + 30)  # 30 days later as YYYYMMDD
#'
#' # Vectorised: convert a column of dates
#' dates <- c(20200101, 20201231, 20210630)
#' iaw$yyyymmdd.toggle(dates)
#'
#' # Count business days between two dates (weekdays only)
#' g1 <- iaw$yyyymmdd.toggle(20240101)
#' g2 <- iaw$yyyymmdd.toggle(20240131)
#' all_days <- seq(g1, g2)
#' weekdays <- iaw$yyyymmdd.toggle(all_days)
#' # (use with yyyymmdd(..., output="weekday") for further filtering)
#'
#' # NA values pass through
#' iaw$yyyymmdd.toggle(c(20240101, NA, 20240301))   # NA preserved in middle

iaw$yyyymmdd.toggle <- function( yyyymmdd_or_int ) {
    if (all( is.na(yyyymmdd_or_int) )) return(yyyymmdd_or_int)
    stopifnot( is.numeric(yyyymmdd_or_int) )

    ## 1970-01-01 is day 0

    if ( all( is.na(yyyymmdd_or_int) | (yyyymmdd_or_int >= 19000000) ) )
        return( as.numeric( as.Date( as.character(yyyymmdd_or_int), format = '%Y%m%d') ) )

    if ( all( is.na(yyyymmdd_or_int) | ((yyyymmdd_or_int <= 50000)&(yyyymmdd_or_int >= -300000)) ))
        return( as.numeric(format(as.Date(yyyymmdd_or_int, origin = "1970-01-01"), "%Y%m%d")) )

    stop("Unrecognized Elements in yyyymmdd.toggle")

}

#' Convert Date-Like Input to Various Formats
#'
#' @name yyyymmdd
#'
#' Converts YYYYMMDD integers, Gregorian day numbers, or POSIXct objects
#' to the requested output format.
#'
#' @param anything Numeric vector (YYYYMMDD or Gregorian) or POSIXct.
#' @param output Output format: one of \code{"posix"}, \code{"gregorian"},
#'   \code{"yyyymmdd"}, \code{"weeknum"}, or \code{"weekday"}.
#'
#' @return Converted date in the requested format.
#'
#' @family datetime
#' @export
#'
#' @seealso [iaw$yyyymmdd.toggle()], [iaw$as.PlotDate.yyyymmdd()]
#'
#' @examples
#' iaw$yyyymmdd(20240115, output = "weekday")   # "Mon"
#'
#' # Convert to Gregorian day number for arithmetic
#' iaw$yyyymmdd(20230101, output = "gregorian")
#'
#' # Get ISO week number
#' iaw$yyyymmdd(20240104, output = "weeknum")
#'
#' # Convert a Gregorian day number back to YYYYMMDD integer
#' iaw$yyyymmdd(19358, output = "yyyymmdd")   # days since 1970-01-01
#'
#' # Determine day of week for a vector of trade dates
#' trade_dates <- c(20240102, 20240103, 20240104)
#' iaw$yyyymmdd(trade_dates, output = "weekday")   # "Tue", "Wed", "Thu"
#'
#' # Convert POSIXct timestamp to YYYYMMDD integer
#' iaw$yyyymmdd(as.POSIXct("2024-06-15 14:30:00"), output = "yyyymmdd")
#'
#' # Week number useful for weekly aggregation
#' iaw$yyyymmdd(20240101, output = "weeknum")
#' iaw$yyyymmdd(20240108, output = "weeknum")   # one week later

iaw$yyyymmdd <- function( anything, output=c("posix", "gregorian", "yyyymmdd", "weeknum", "weekday") ) {

    if (length(output)!=1) {
        message("Please give desired output format, one of ")
        print(output)
        stop("choose output format in call")
    }

    if (all( is.na(anything) )) return(anything)

    ## first convert other formats, like yyyymmdd or 10000 (=19970519) into a string

    if ( ! inherits(anything, "POSIXct") ) {
        stopifnot( is.numeric( anything ) )
        if ( all( is.na(anything) | (anything >= 19000000) ) ) {
            anything <- as.Date( as.character(anything), format = '%Y%m%d')
        } else if ( all( is.na(anything) | ((anything <= 50000)&(anything >= -100000)) )) {
            anything <- as.Date(anything, origin = "1970-01-01")
        }
    }

    if (output == "posix") {
        return(anything)
    } else if (output == "gregorian") {
        return(as.numeric(anything))
    } else if (output %in% c("weeknum","wnum")) {
        return(as.integer( (as.numeric(anything)-4)/7 ))
        ## NOTE: dead assertion removed (was after return):
        ## stopifnot( iaw$yyyymmdd( c(20240602, 20240603), output="wnum") == c(2838,2839) ) ## week begins on Monday
    } else if (output %in% c("weekday","wday")) {
        return(format(anything, format="%a"))
    }
    stop(paste0("don't know desired output format ",output))
}
