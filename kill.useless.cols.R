
#' KILL.USELESS.COLS
#'
#' @name kill.useless.cols
#'
#' remove all columns in a data frame that have no variance
#'
#' @usage kill.useless.cols (d)
#'
#' @param d a data frame
#'
#' @return a new dataframe
#'

iaw$kill.useless.cols <- function (d) {
  to_keep <- sapply(d, function(col) length(unique(col)) > 1)
  d[, to_keep, drop=FALSE]
}

