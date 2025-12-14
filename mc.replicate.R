#' Parallel Replicate
#'
#' A parallel version of \code{replicate()} that repeats an expression
#' multiple times using parallel processing.
#'
#' @param n Integer; number of replications.
#' @param expr Expression to evaluate repeatedly.
#' @param simplify Character or logical; how to simplify results.
#'   Default "array" tries to combine into an array.
#'
#' @return Simplified results if possible, otherwise a list.
#'
#' @details
#' Unlike \code{rep()}, which repeats a value, \code{mc.replicate()} repeats
#' a function call. Each call is executed in parallel using multiple cores.
#'
#' This is useful for Monte Carlo simulations, bootstrapping, and other
#' tasks requiring many independent evaluations.
#'
#' @export
#'
#' @seealso \code{\link{replicate}}, \code{\link{iaw$mcsapply}}
#'
#' @examples
#' # Monte Carlo estimation of pi
#' estimate_pi <- function() {
#'     n <- 10000
#'     x <- runif(n)
#'     y <- runif(n)
#'     4 * mean(x^2 + y^2 < 1)
#' }
#' estimates <- iaw$mc.replicate(100, estimate_pi())
#' mean(estimates)  # Should be close to pi
#'
#' # Bootstrap standard error
#' x <- rnorm(100)
#' boot_means <- iaw$mc.replicate(1000, mean(sample(x, replace = TRUE)))
#' sd(boot_means)

iaw$mc.replicate <- function(n, expr, simplify = "array") {
    iaw$mcsapply(
        integer(n),
        eval.parent(substitute(function(...) expr)),
        simplify = simplify
    )
}
