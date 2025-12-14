#' Unix Epoch Time Conversions
#'
#' Functions to convert between Unix epoch timestamps and R date/time objects.
#'
#' @name epoch-conversions
#' @rdname epoch-conversions
#'
#' @param unixepochtime Numeric; Unix epoch time (seconds since 1970-01-01).
#' @param epochin Numeric; Unix epoch time to convert.
#'
#' @return
#' \itemize{
#'   \item \code{epoch2POSIXct}: POSIXct object
#'   \item \code{epoch2yyyymmdd}: Integer in YYYYMMDD format
#'   \item \code{epoch2hhmmss}: Integer in HHMMSS format
#' }
#'
#' @export
#'
#' @examples
#' # Current epoch time
#' epoch <- as.numeric(Sys.time())
#'
#' # Convert to POSIXct
#' iaw$epoch2POSIXct(epoch)
#'
#' # Convert to date integer
#' iaw$epoch2yyyymmdd(epoch)
#'
#' # Convert to time integer
#' iaw$epoch2hhmmss(epoch)

#' @rdname epoch-conversions
#' @export
iaw$epoch2POSIXct <- function(unixepochtime) {
    structure(unixepochtime, class = c("POSIXt", "POSIXct"))
}

#' @rdname epoch-conversions
#' @export
iaw$epoch2yyyymmdd <- function(epochin) {
    as.integer(strftime(
        as.POSIXct(epochin, origin = "1970-01-01", tz = "EST5EDT"),
        format = "%Y%m%d"
    ))
}

#' @rdname epoch-conversions
#' @export
iaw$epoch2hhmmss <- function(epochin) {
    as.integer(strftime(
        as.POSIXct(epochin, origin = "1970-01-01", tz = "EST5EDT"),
        format = "%H%M%S"
    ))
}
