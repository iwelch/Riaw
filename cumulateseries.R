#' Cumulative Sum of a Time Series
#'
#' Computes the cumulative sum of a numeric series. This is a simple wrapper
#' around \code{cumsum()} with input validation.
#'
#' @param seriesin A numeric vector.
#'
#' @return A numeric vector of the same length containing cumulative sums.
#'
#' @export
#'
#' @seealso \code{\link{cumsum}}, \code{\link{iaw$compoundseries}}
#'
#' @examples
#' # Simple cumulative sum
#' x <- c(1, 2, 3, 4, 5)
#' iaw$cumulateseries(x)
#' # 1 3 6 10 15
#'
#' # Cumulative returns (log returns)
#' log_returns <- c(0.01, -0.02, 0.03, 0.01)
#' cumulative_log_return <- iaw$cumulateseries(log_returns)
#' # Convert back: exp(cumulative_log_return) - 1

iaw$cumulateseries <- function(seriesin) {
    (is.numeric(seriesin)) %or% "seriesin must be numeric, not {{class(seriesin)}}"
    (length(seriesin) >= 1) %or% "Need at least 1 observation"
    cumsum(seriesin)
}
