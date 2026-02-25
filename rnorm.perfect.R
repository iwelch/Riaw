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

iaw$rnorm.perfect <- function(n, mean = 0, sd = 1) {
    stopifnot(is.numeric(n), length(n) == 1L, n >= 1)
    stopifnot(is.numeric(mean), length(mean) == 1L)
    stopifnot(is.numeric(sd), length(sd) == 1L, sd > 0)
    
    iaw$rdraw.perfect(n, function(p) qnorm(p, mean = mean, sd = sd))
}
