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
#'
#' # Count valid observations in a returns matrix (e.g., stocks with IPO gaps)
#' returns <- data.frame(SPY = c(0.01, 0.02, -0.01), NEWCO = c(NA, NA, 0.03))
#' iaw$colN(returns)  # SPY=3, NEWCO=1
#'
#' # Single-column matrix still returns a named count
#' iaw$colN(matrix(c(1, NA, 3), ncol = 1, dimnames = list(NULL, "col1")))  # col1=2
#'
#' # Completeness ratio: fraction of non-missing per column
#' m <- matrix(c(1, NA, 3, 4, NA, NA), nrow = 3)
#' iaw$colN(m) / nrow(m)  # 0.667, 0.333

iaw$colN <- function(x) {
    setNames(as.integer(colSums(!is.na(x))), colnames(x))
}
