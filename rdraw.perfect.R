#' Draw from Distribution with Exact Quantiles
#'
#' @name rdraw.perfect
#'
#' Generates values matching theoretical quantiles.
#'
#' @param n Number of values.
#' @param qfun Quantile function.
#'
#' @return Numeric vector.
#'
#' @family utilities
#' @export

iaw$rdraw.perfect <- function(n, qfun = qnorm) {
    stopifnot(is.numeric(n), length(n) == 1L, n >= 1)
    stopifnot(is.function(qfun))
    
    p <- (seq_len(n) - 0.5) / n
    qfun(p)
}
