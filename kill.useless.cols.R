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
#' # Column b is constant: removed; a is kept
#' df <- data.frame(a = 1:3, b = c(1, 1, 1))
#' iaw$kill.useless.cols(df)
#'
#' # Multiple constant columns removed at once
#' df2 <- data.frame(x = rnorm(5), flag = TRUE, id = 99L, y = rnorm(5))
#' iaw$kill.useless.cols(df2)   # only x and y survive
#'
#' # All-NA column is treated as constant (one unique value: NA)
#' df3 <- data.frame(a = c(1, 2, 3), b = c(NA, NA, NA))
#' iaw$kill.useless.cols(df3)   # b removed
#'
#' # Clean a financial dataset before regression
#' stocks <- data.frame(
#'   ret     = c(0.01, -0.02, 0.03),
#'   country = c("US", "US", "US"),   # constant -> removed
#'   exchg   = c(1, 1, 1),            # constant -> removed
#'   vol     = c(100, 200, 150)
#' )
#' iaw$kill.useless.cols(stocks)   # keeps ret and vol only
#'
#' # Single-row data frame: all columns are "constant"
#' iaw$kill.useless.cols(data.frame(a = 1, b = "x"))
#' # returns 0-column data frame
#'
#' # Preserves columns with at least one difference
#' df4 <- data.frame(a = c(1, 1, 2), b = c(5, 5, 5))
#' ncol(iaw$kill.useless.cols(df4))  # 1

iaw$kill.useless.cols <- function(d) {
    stopifnot(is.data.frame(d))
    d[, sapply(d, function(col) length(unique(col)) > 1), drop = FALSE]
}
