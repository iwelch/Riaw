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
#' @family utilities
#' @export

iaw$rexp.perfect <- function(n, rate = 1) {
    stopifnot(is.numeric(n), length(n) == 1L, n >= 1)
    stopifnot(is.numeric(rate), length(rate) == 1L, rate > 0)
    
    iaw$rdraw.perfect(n, function(p) qexp(p, rate = rate))
}
