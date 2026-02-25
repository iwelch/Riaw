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
#' # Data frame with missing values in one column
#' df <- data.frame(a = c(1, NA, 3), b = c(1, 2, 3))
#' iaw$colN(df)   # a=2, b=3
#'
#' # Matrix with scattered NAs
#' m <- matrix(c(1, NA, 3, NA, 5, 6), nrow = 2)
#' iaw$colN(m)
#'
#' # All-NA column reports zero
#' df2 <- data.frame(x = c(NA, NA), y = c(1, 2))
#' iaw$colN(df2)  # x=0, y=2

iaw$colN <- function(x) {
    setNames(as.integer(colSums(!is.na(x))), colnames(x))
}
