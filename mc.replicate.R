#' Parallel Replicate
#'
#' @name mc.replicate
#'
#' Parallel version of replicate.
#'
#' @param n Number of replications.
#' @param expr Expression to evaluate.
#' @param simplify Simplify result.
#'
#' @return Vector or list of results.
#'
#' @family parallel
#' @export
#'
#' @examples
#' \dontrun{
#' iaw$mc.replicate(100, mean(rnorm(100)))
#' }

iaw$mc.replicate <- function(n, expr, simplify = TRUE) {
    stopifnot(is.numeric(n), length(n) == 1L, n > 0)
    
    result <- iaw$mclapply(seq_len(n), function(i) eval(expr))
    if (simplify) simplify2array(result) else result
}
