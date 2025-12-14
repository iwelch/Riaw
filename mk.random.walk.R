#' Generate Random Walk
#'
#' @name mk.random.walk
#'
#' Creates random walk series.
#'
#' @param n Length of series.
#' @param start Starting value.
#' @param sd Standard deviation of steps.
#'
#' @return Numeric vector.
#'
#' @family utilities
#' @export
#'
#' @examples
#' iaw$mk.random.walk(100)

iaw$mk.random.walk <- function(n, start = 0, sd = 1) {
    stopifnot(is.numeric(n), length(n) == 1L, n >= 1)
    stopifnot(is.numeric(start), length(start) == 1L)
    stopifnot(is.numeric(sd), length(sd) == 1L, sd > 0)
    
    cumsum(c(start, rnorm(n - 1, sd = sd)))
}
