#' Population Variance
#'
#' @name varp
#'
#' Calculates population variance (n denominator).
#'
#' @param x Numeric vector.
#' @param na.rm Remove NA values.
#'
#' @return Variance value.
#'
#' @family statistics
#' @export
#'
#' @examples
#' # Population variance (divides by n, not n-1)
#' iaw$varp(1:10)
#' var(1:10)        # sample variance is larger
#'
#' # For a population of N elements, varp = var * (N-1)/N
#' x <- c(2, 4, 4, 4, 5, 5, 7, 9)
#' iaw$varp(x)
#' var(x) * (length(x) - 1) / length(x)   # same result
#'
#' # NA values are dropped by default
#' iaw$varp(c(1, 2, NA, 4, 5))
#'
#' # Population variance of daily returns
#' set.seed(1)
#' returns <- rnorm(252, mean = 0.0004, sd = 0.01)
#' iaw$varp(returns)   # slightly less than var(returns)
#'
#' # Single element has zero variance
#' iaw$varp(42)   # 0
#'
#' # Constant vector has zero variance
#' iaw$varp(rep(3.14, 100))   # 0
#'
#' # Explicit na.rm = FALSE propagates NA
#' iaw$varp(c(1, NA, 3), na.rm = FALSE)   # NA

iaw$varp <- function(x, na.rm = TRUE) {
    stopifnot(is.numeric(x))
    if (na.rm) x <- x[!is.na(x)]
    n <- length(x)
    if (n == 0) return(NA_real_)
    if (n == 1) return(0)
    var(x) * (n - 1) / n
}
