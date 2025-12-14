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
#' @family statistics
#' @export

iaw$sse <- function(actual, predicted) {
    stopifnot(is.numeric(actual), is.numeric(predicted))
    stopifnot(length(actual) == length(predicted))
    sum((actual - predicted)^2, na.rm = TRUE)
}
