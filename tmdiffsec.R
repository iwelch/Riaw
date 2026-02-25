#' Time Difference in Seconds
#'
#' @name tmdiffsec
#'
#' Calculates time difference in seconds.
#'
#' @param t1 First time.
#' @param t2 Second time.
#'
#' @return Numeric seconds.
#'
#' @examples
#' t1 <- as.POSIXct("2024-01-01 10:00:00")
#' t2 <- as.POSIXct("2024-01-01 10:05:30")
#'
#' # Elapsed seconds between two timestamps
#' iaw$tmdiffsec(t1, t2)   # 330
#'
#' # Measure wall-clock time for a computation
#' start <- Sys.time()
#' Sys.sleep(0.1)
#' iaw$tmdiffsec(start, Sys.time())   # approximately 0.1
#'
#' # Difference across midnight
#' t_before <- as.POSIXct("2024-06-15 23:59:50")
#' t_after  <- as.POSIXct("2024-06-16 00:00:10")
#' iaw$tmdiffsec(t_before, t_after)   # 20
#'
#' # Negative result when t2 is earlier than t1
#' iaw$tmdiffsec(t_after, t_before)   # -20
#'
#' # Convert to minutes by dividing
#' t1 <- as.POSIXct("2024-01-01 09:00:00")
#' t2 <- as.POSIXct("2024-01-01 10:30:00")
#' iaw$tmdiffsec(t1, t2) / 60   # 90 minutes
#'
#' @family datetime
#' @export

iaw$tmdiffsec <- function(t1, t2) {
    as.numeric(difftime(t2, t1, units = "secs"))
}
