#' Convert Unix Epoch to YYYYMMDD Integer
#'
#' @name epoch2yyyymmdd
#'
#' Converts Unix timestamp to integer date.
#'
#' @param epoch Numeric Unix timestamp.
#' @param tz Timezone. Default "UTC".
#'
#' @return Integer YYYYMMDD.
#'
#' @family datetime
#' @export
#'
#' @examples
#' iaw$epoch2yyyymmdd(1609459200)   # 20210101 (2021-01-01 00:00 UTC)
#'
#' # Convert a vector of timestamps
#' iaw$epoch2yyyymmdd(c(0, 86400, 1000000000))   # 19700101, 19700102, 20010909
#'
#' # Extract HHMMSS from the same timestamp
#' iaw$epoch2hhmmss(1609459200 + 3723)   # 10123 -> 01:01:23 UTC
#'
#' # Use a local timezone instead of UTC
#' iaw$epoch2yyyymmdd(1609459200, tz = "America/New_York")  # still 20201231 EST

iaw$epoch2yyyymmdd <- function(epoch, tz = "UTC") {
    stopifnot(is.numeric(epoch))
    dt <- as.POSIXct(epoch, origin = "1970-01-01", tz = tz)
    as.integer(format(dt, "%Y%m%d"))
}

#' @rdname epoch2yyyymmdd
#'
#' @return \code{epoch2hhmmss}: Integer HHMMSS.
#'
#' @export
iaw$epoch2hhmmss <- function(epoch, tz = "UTC") {
    stopifnot(is.numeric(epoch))
    dt <- as.POSIXct(epoch, origin = "1970-01-01", tz = tz)
    as.integer(format(dt, "%H%M%S"))
}
