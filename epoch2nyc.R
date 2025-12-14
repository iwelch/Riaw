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
#' @family datetime
#' @export

iaw$epoch2nyc <- function(epoch) {
    iaw$epoch2POSIXct(epoch, tz = "America/New_York")
}
