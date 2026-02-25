#' Perfect Exponential Sample
#'
#' @name rexp.perfect
#'
#' Generates exponential values with exact quantiles.
#'
#' @param n Number of values.
#' @param rate Rate parameter.
#'
#' @return Numeric vector.
#'
#' @examples
#' # 10 exponential draws with exact quantile spacing (rate = 1)
#' x <- iaw$rexp.perfect(10)
#' x
#'
#' # Higher rate parameter (shorter mean)
#' x2 <- iaw$rexp.perfect(8, rate = 2)
#' x2
#'
#' # The sample mean matches the theoretical mean 1/rate
#' mean(iaw$rexp.perfect(1000, rate = 0.5))   # close to 2.0
#'
#' @family utilities
#' @export

iaw$rexp.perfect <- function(n, rate = 1) {
    stopifnot(is.numeric(n), length(n) == 1L, n >= 1)
    stopifnot(is.numeric(rate), length(rate) == 1L, rate > 0)
    
    iaw$rdraw.perfect(n, function(p) qexp(p, rate = rate))
}
