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
#' # Default: 100-step walk starting at 0 with SD = 1
#' set.seed(42)
#' rw <- iaw$mk.random.walk(100)
#' length(rw)   # 100
#'
#' # Start from a non-zero level
#' iaw$mk.random.walk(10, start = 100)
#'
#' # Volatile walk: larger step size
#' set.seed(1)
#' iaw$mk.random.walk(10, sd = 5)
#'
#' # Simulate two independent price paths
#' set.seed(7)
#' p1 <- iaw$mk.random.walk(50, start = 100, sd = 1)
#' p2 <- iaw$mk.random.walk(50, start = 100, sd = 2)
#' cor(p1, p2)   # near zero for independent walks
#'
#' # Walk of length 1 is just the starting value
#' iaw$mk.random.walk(1, start = 42)  # 42
#'
#' # Simulate a stylized stock price over 252 trading days
#' set.seed(2024)
#' daily_price <- iaw$mk.random.walk(252, start = 100, sd = 1.5)
#' range(daily_price)  # approximate price range over one year
#'
#' # Multiple walks for a Monte Carlo fan chart
#' set.seed(99)
#' paths <- replicate(50, iaw$mk.random.walk(100, start = 0, sd = 0.5))
#' dim(paths)  # 100 x 50 matrix of simulated paths

iaw$mk.random.walk <- function(n, start = 0, sd = 1) {
    stopifnot(is.numeric(n), length(n) == 1L, n >= 1)
    stopifnot(is.numeric(start), length(start) == 1L)
    stopifnot(is.numeric(sd), length(sd) == 1L, sd > 0)
    
    cumsum(c(start, rnorm(n - 1, sd = sd)))
}
