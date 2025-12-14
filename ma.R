#' Moving Average
#'
#' Calculates the moving average of a numeric vector.
#'
#' @param x Numeric vector.
#' @param n Integer; window size for the moving average. Default 5.
#' @param around Logical; if TRUE, uses centered moving average (looks both
#'   forward and backward). If FALSE (default), uses trailing average.
#'
#' @return A numeric vector of the same length as \code{x}. Contains NA values
#'   at the boundaries where the full window isn't available.
#'
#' @export
#'
#' @seealso \code{\link{filter}}, \code{\link{iaw$compoundseries}}
#'
#' @examples
#' x <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
#'
#' # 3-period trailing moving average
#' iaw$ma(x, n = 3)
#' # NA NA 2 3 4 5 6 7 8 9
#'
#' # 3-period centered moving average
#' iaw$ma(x, n = 3, around = TRUE)
#' # NA 2 3 4 5 6 7 8 9 NA
#'
#' # Smooth noisy data
#' noisy <- sin(1:100) + rnorm(100, sd = 0.3)
#' smoothed <- iaw$ma(noisy, n = 5)

iaw$ma <- function(x, n = 5, around = FALSE) {
    as.numeric(filter(x, rep(1/n, n), sides = if (around) 2 else 1))
}
