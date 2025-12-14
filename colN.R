#' Column Count Non-NA
#'
#' @name colN
#'
#' Counts non-NA values in each column.
#'
#' @param x Matrix or data frame.
#'
#' @return Vector of counts.
#'
#' @family statistics
#' @export
#'
#' @examples
#' df <- data.frame(a = c(1, NA, 3), b = c(1, 2, 3))
#' iaw$colN(df)

iaw$colN <- function(x) {
    setNames(as.integer(colSums(!is.na(x))), colnames(df))
}
