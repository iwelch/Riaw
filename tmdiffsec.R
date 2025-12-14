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
#' @family datetime
#' @export

iaw$tmdiffsec <- function(t1, t2) {
    as.numeric(difftime(t2, t1, units = "secs"))
}
