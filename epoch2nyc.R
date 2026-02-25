#' Convert Unix Epoch to NYC Time
#'
#' @name epoch2nyc
#'
#' Converts Unix timestamp to NYC timezone.
#'
#' @param epoch Numeric Unix timestamp.
#'
#' @return POSIXct in America/New_York.
#'
#' @examples
#' # Unix epoch 0 is 1969-12-31 19:00:00 EST
#' iaw$epoch2nyc(0)
#'
#' # Convert a known timestamp: 2021-01-01 00:00:00 UTC = 2020-12-31 19:00:00 EST
#' iaw$epoch2nyc(1609459200)
#'
#' # Convert a vector of timestamps
#' ts <- c(1609459200, 1609545600)
#' iaw$epoch2nyc(ts)
#'
#' @family datetime
#' @export

iaw$epoch2nyc <- function(epoch) {
    iaw$epoch2POSIXct(epoch, tz = "America/New_York")
}
