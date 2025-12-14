#' Current Timestamp
#'
#' @name now
#'
#' Returns current time as string.
#'
#' @param format Time format string.
#'
#' @return Character timestamp.
#'
#' @family utilities
#' @export
#'
#' @examples
#' iaw$now()

iaw$now <- function(format = "%Y-%m-%d %H:%M:%S") {
    stopifnot(is.character(format), length(format) == 1L)
    format(Sys.time(), format)
}
