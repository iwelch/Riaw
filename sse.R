#' Sum of Squared Errors
#'
#' @name sse
#'
#' Calculates sum of squared errors.
#'
#' @param actual Actual values.
#' @param predicted Predicted values.
#'
#' @return SSE value.
#'
#' @examples
#' actual    <- c(1, 2, 3, 4, 5)
#' predicted <- c(1.1, 1.9, 3.2, 3.8, 5.1)
#'
#' # Compute sum of squared errors
#' iaw$sse(actual, predicted)   # small positive number
#'
#' # Perfect predictions give SSE of 0
#' iaw$sse(actual, actual)      # 0
#'
#' # NA values are dropped automatically
#' iaw$sse(c(1, NA, 3), c(1, 2, 4))   # (3-4)^2 = 1
#'
#' @family statistics
#' @export

iaw$sse <- function(actual, predicted) {
    stopifnot(is.numeric(actual), is.numeric(predicted))
    stopifnot(length(actual) == length(predicted))
    sum((actual - predicted)^2, na.rm = TRUE)
}
