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
#' # Estimate the sampling distribution of the mean via parallel simulation
#' means <- iaw$mc.replicate(1000, mean(rnorm(100)))
#' hist(means)
#'
#' # Bootstrap a correlation coefficient
#' x <- rnorm(50); y <- x + rnorm(50)
#' boot_cors <- iaw$mc.replicate(500, {
#'   idx <- sample(50, replace = TRUE)
#'   cor(x[idx], y[idx])
#' })
#' quantile(boot_cors, c(0.025, 0.975))  # 95% bootstrap CI
#'
#' # Return a list (simplify = FALSE) when each replication yields a vector
#' sims <- iaw$mc.replicate(200, rnorm(5), simplify = FALSE)
#' length(sims)  # 200 elements, each of length 5
#' }

iaw$mc.replicate <- function(n, expr, simplify = TRUE) {
    stopifnot(is.numeric(n), length(n) == 1L, n > 0)
    expr <- substitute(expr)
    penv <- parent.frame()
    result <- iaw$mclapply(seq_len(n), function(i) eval(expr, envir = penv))
    if (simplify) simplify2array(result) else result
}
