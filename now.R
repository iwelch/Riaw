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
#'
#' # ISO 8601 format with timezone
#' iaw$now("%Y-%m-%dT%H:%M:%S%z")  # "2024-03-15T09:32:07-0500"
#'
#' # Hour and minute only (e.g., for intraday trading logs)
#' iaw$now("%H:%M")                 # "09:32"
#'
#' # Month-Year label for report headers
#' iaw$now("%B %Y")                 # "March 2024"

iaw$now <- function(format = "%Y-%m-%d %H:%M:%S") {
    stopifnot(is.character(format), length(format) == 1L)
    format(Sys.time(), format)
}
