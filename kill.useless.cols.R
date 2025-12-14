#' Remove Columns with No Variance
#'
#' Removes all columns from a data frame that have only one unique value
#' (i.e., no variance). Useful for cleaning data before analysis.
#'
#' @param d A data frame.
#'
#' @return A data frame with constant columns removed.
#'
#' @export
#'
#' @seealso \code{\link{iaw$completeobs}}
#'
#' @examples
#' df <- data.frame(
#'     a = c(1, 2, 3, 4),
#'     b = c(5, 5, 5, 5),  # constant - will be removed
#'     c = c("x", "y", "x", "z"),
#'     d = c(NA, NA, NA, NA)  # all NA - will be removed
#' )
#'
#' iaw$kill.useless.cols(df)
#' #   a c
#' # 1 1 x
#' # 2 2 y
#' # 3 3 x
#' # 4 4 z

iaw$kill.useless.cols <- function(d) {
    to_keep <- sapply(d, function(col) length(unique(col)) > 1)
    d[, to_keep, drop = FALSE]
}
