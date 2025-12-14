#' Get Complete Observations
#'
#' @name completeobs
#'
#' Returns rows with no NA values.
#'
#' @param d Data frame.
#'
#' @return Data frame with complete cases.
#'
#' @family data-manipulation
#' @export
#'
#' @examples
#' df <- data.frame(a = c(1, NA, 3), b = c(4, 5, 6))
#' iaw$completeobs(df)

iaw$completeobs <- function(d) {
    stopifnot(is.data.frame(d))
    d[complete.cases(d), ]
}
