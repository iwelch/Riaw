#' Bisection Root Finding
#'
#' @name bisection
#'
#' Finds root using bisection method.
#'
#' @param f Function to find root of.
#' @param a Lower bound.
#' @param b Upper bound.
#' @param tol Tolerance.
#' @param maxiter Maximum iterations.
#'
#' @return Root value.
#'
#' @family utilities
#' @export
#'
#' @examples
#' iaw$bisection(function(x) x^2 - 2, 0, 2)

iaw$bisection <- function(f, a, b, tol = 1e-9, maxiter = 100) {
    stopifnot(is.function(f))
    stopifnot(is.numeric(a), is.numeric(b))
    stopifnot(is.numeric(tol), tol > 0)
    stopifnot(is.numeric(maxiter), maxiter > 0)
    
    for (i in seq_len(maxiter)) {
        mid <- (a + b) / 2
        if (abs(f(mid)) < tol || (b - a) / 2 < tol) {
            return(mid)
        }
        if (sign(f(mid)) == sign(f(a))) {
            a <- mid
        } else {
            b <- mid
        }
    }
    warning("Maximum iterations reached")
    mid
}
