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
#' # Regression residual SSE matches sum of squared residuals
#' set.seed(42)
#' y <- 1:20 + rnorm(20)
#' fit <- lm(y ~ seq_along(y))
#' iaw$sse(y, fitted(fit))   # same as sum(resid(fit)^2)
#'
#' # Compare two competing forecasts
#' actual <- c(100, 102, 105, 103, 108)
#' model_a <- c(101, 101, 106, 104, 107)
#' model_b <- c(99, 103, 104, 102, 110)
#' iaw$sse(actual, model_a)   # 4
#' iaw$sse(actual, model_b)   # 10 -- model A is better
#'
#' # Single observation
#' iaw$sse(5, 3)   # 4
#'
#' @family statistics
#' @export

iaw$sse <- function(actual, predicted) {
    stopifnot(is.numeric(actual), is.numeric(predicted))
    stopifnot(length(actual) == length(predicted))
    sum((actual - predicted)^2, na.rm = TRUE)
}
