#' Perfect Normal Sample
#'
#' @name rnorm.perfect
#'
#' Generates normal values with exact quantiles.
#'
#' @param n Number of values.
#' @param mean Mean.
#' @param sd Standard deviation.
#'
#' @return Numeric vector.
#'
#' @family utilities
#' @export
#'
#' @examples
#' x <- iaw$rnorm.perfect(100)
#' mean(x)  # Exactly 0 (by construction)
#' sd(x)    # Exactly 1
#'
#' # Custom mean and sd
#' y <- iaw$rnorm.perfect(50, mean = 10, sd = 2)
#' mean(y)  # Exactly 10
#' sd(y)    # Exactly 2
#'
#' # Useful for simulation studies that need exact moments
#' z <- iaw$rnorm.perfect(1000)
#' range(z)   # Wider tails than rnorm() because quantiles are exact
#'
#' # Simulate daily stock returns with realistic parameters
#' daily_ret <- iaw$rnorm.perfect(252, mean = 0.0003, sd = 0.015)
#' round(mean(daily_ret) * 252, 4)  # annualized mean ~0.0756
#'
#' # Verify skewness is near zero (symmetric by construction)
#' x <- iaw$rnorm.perfect(500)
#' round(mean(((x - mean(x)) / sd(x))^3), 4)  # skewness ~0
#'
#' # Use as benchmark input where exact moments matter
#' x <- iaw$rnorm.perfect(100, mean = 5, sd = 3)
#' round(c(mean = mean(x), sd = sd(x)), 4)  # 5.0000, 3.0000

iaw$rnorm.perfect <- function(n, mean = 0, sd = 1) {
    stopifnot(is.numeric(n), length(n) == 1L, n >= 1)
    stopifnot(is.numeric(mean), length(mean) == 1L)
    stopifnot(is.numeric(sd), length(sd) == 1L, sd > 0)
    
    iaw$rdraw.perfect(n, function(p) qnorm(p, mean = mean, sd = sd))
}
