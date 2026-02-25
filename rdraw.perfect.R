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
#'
#' @examples
#' # Normal draws with exact quantiles
#' x <- iaw$rdraw.perfect(100, qnorm)
#' mean(x)  # Very close to 0
#' sd(x)    # Very close to 1
#'
#' # Uniform draws covering [0, 1] evenly
#' u <- iaw$rdraw.perfect(10, qunif)
#' sort(u)   # 0.05, 0.15, ..., 0.95
#'
#' # Exponential draws
#' e <- iaw$rdraw.perfect(50, qexp)
#' mean(e)  # Close to 1 (theoretical mean of Exp(1))
#'
#' # Custom quantile function via closure
#' q_t5 <- function(p) qt(p, df = 5)
#' iaw$rdraw.perfect(20, q_t5)

iaw$rdraw.perfect <- function(n, qfun = qnorm) {
    stopifnot(is.numeric(n), length(n) == 1L, n >= 1)
    stopifnot(is.function(qfun))
    
    p <- (seq_len(n) - 0.5) / n
    sample(qfun(p))
}
