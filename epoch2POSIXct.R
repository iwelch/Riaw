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
#' iaw$epoch2POSIXct(1609459200)

iaw$epoch2POSIXct <- function(epoch, tz = "UTC") {
    stopifnot(is.numeric(epoch))
    as.POSIXct(epoch, origin = "1970-01-01", tz = tz)
}
