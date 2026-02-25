#' Convert Unix Epoch to POSIXct
#'
#' @name epoch2POSIXct
#'
#' Converts Unix timestamp to POSIXct.
#'
#' @param epoch Numeric Unix timestamp.
#' @param tz Timezone. Default "UTC".
#'
#' @return POSIXct datetime.
#'
#' @family datetime
#' @export
#'
#' @examples
#' # 2021-01-01 00:00:00 UTC
#' iaw$epoch2POSIXct(1609459200)
#'
#' # Convert with a specific timezone
#' iaw$epoch2POSIXct(1609459200, tz = "America/New_York")
#'
#' # Vectorized: convert a column of timestamps
#' ts <- c(1609459200, 1612137600, 1614556800)
#' iaw$epoch2POSIXct(ts)
#'
#' # Epoch 0 is the Unix origin
#' iaw$epoch2POSIXct(0)  # "1970-01-01 UTC"
#'
#' # Convert trade timestamps to Tokyo time for Asian session analysis
#' iaw$epoch2POSIXct(1609459200, tz = "Asia/Tokyo")  # 2021-01-01 09:00:00 JST
#'
#' # Sub-second precision: millisecond timestamps (divide by 1000 first)
#' ms_epoch <- 1609459200123
#' iaw$epoch2POSIXct(ms_epoch / 1000)  # fractional seconds preserved
#'
#' # Round-trip: POSIXct back to epoch
#' dt <- iaw$epoch2POSIXct(1609459200)
#' as.numeric(dt)  # 1609459200

iaw$epoch2POSIXct <- function(epoch, tz = "UTC") {
    stopifnot(is.numeric(epoch))
    as.POSIXct(epoch, origin = "1970-01-01", tz = tz)
}
