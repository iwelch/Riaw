#' Remove Constant Columns
#'
#' @name kill.useless.cols
#'
#' Removes columns with no variance.
#'
#' @param d Data frame.
#'
#' @return Data frame with constant columns removed.
#'
#' @family data-manipulation
#' @export
#'
#' @examples
#' df <- data.frame(a = 1:3, b = c(1,1,1))
#' iaw$kill.useless.cols(df)

iaw$kill.useless.cols <- function(d) {
    stopifnot(is.data.frame(d))
    d[, lapply(.SD, function(x) length(unique(x)) > 1)]
}
