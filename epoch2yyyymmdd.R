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
#' iaw$epoch2yyyymmdd(1609459200)

iaw$epoch2yyyymmdd <- function(epoch, tz = "UTC") {
    stopifnot(is.numeric(epoch))
    dt <- as.POSIXct(epoch, origin = "1970-01-01", tz = tz)
    as.integer(format(dt, "%Y%m%d"))
}
