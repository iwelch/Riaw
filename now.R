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
#' iaw$now()                          # "2024-03-15 09:32:07"
#'
#' # Date only
#' iaw$now("%Y-%m-%d")
#'
#' # Compact timestamp for filenames
#' iaw$now("%Y%m%d_%H%M%S")
#'
#' # Log prefix
#' paste0("[", iaw$now(), "] Starting job")

iaw$now <- function(format = "%Y-%m-%d %H:%M:%S") {
    stopifnot(is.character(format), length(format) == 1L)
    format(Sys.time(), format)
}
