#' Vectorized Bisection Root-Finder
#'
#' Finds roots of a function using the bisection method. Unlike \code{uniroot},
#' this function is fully vectorized and can find multiple roots simultaneously.
#'
#' @param f A function of one variable (plus optional additional arguments).
#' @param lower Numeric vector of lower bounds for the search.
#' @param upper Numeric vector of upper bounds for the search.
#' @param ... Additional arguments passed to \code{f}.
#' @param numiter Maximum number of iterations. Default 100.
#' @param tolerance Convergence tolerance. Default \code{.Machine$double.eps^0.25}.
#' @param verbose Integer; if > 0, prints progress information.
#'
#' @return A list with components:
#'   \itemize{
#'     \item \code{mid}: Vector of root estimates
#'     \item \code{fmid}: Function values at roots
#'     \item \code{lower}, \code{upper}: Final bounds
#'     \item \code{flower}, \code{fupper}: Function values at bounds
#'     \item \code{n}: Number of iterations used
#'   }
#'
#' @details
#' Requires that \code{f(lower)} and \code{f(upper)} have opposite signs for
#' each element (guaranteeing a root exists in the interval).
#'
#' @export
#'
#' @seealso \code{\link{uniroot}}, \code{\link{optimize}}
#'
#' @examples
#' # Find roots of x^2 - c for different values of c
#' f <- function(x, c) x^2 - c
#' result <- iaw$bisection(f, lower = rep(0, 5), upper = rep(10, 5), c = 1:5)
#' result$mid
#' # Should be approximately sqrt(1:5)
#'
#' # Implied volatility example (conceptual)
#' # bs_price <- function(sigma, S, K, r, T, market_price) {
#' #     theoretical_price(S, K, r, T, sigma) - market_price
#' # }
#' # iaw$bisection(bs_price, lower = 0.01, upper = 2.0, S=100, K=100, r=0.05, T=1, market_price=10)

iaw$bisection <- function(f, lower, upper, ..., numiter = 100,
                           tolerance = .Machine$double.eps^0.25, verbose = 0) {
    if (!is.null(getOption("strict"))) {
        (is.function(f)) %or% "f must be a function"
        (is.numeric(lower)) %or% "lower must be numeric"
        (is.numeric(upper)) %or% "upper must be numeric"
        (length(upper) == length(lower)) %or%
            "length of lower {{length(lower)}} must equal length of upper {{length(upper)}}"
    }

    flower <- f(lower, ...)
    (all(is.numeric(flower))) %or% "f(lower) must return numeric"
    (all(!is.na(flower))) %or% "f(lower) contains NA values"

    fupper <- f(upper, ...)
    (all(is.numeric(fupper))) %or% "f(upper) must return numeric"
    (all(!is.na(fupper))) %or% "f(upper) contains NA values"

    problem.cases <- which((fupper * flower) > 0)
    (length(problem.cases) == 0) %or%
        "{{length(problem.cases)}} cases have same sign at both bounds"

    # Bisection iteration
    for (n in 1:numiter) {
        mid <- (lower + upper) / 2
        fmid <- f(mid, ...)

        if (verbose) {
            cat("[bisection:", n, "/", numiter, "=",
                mean(abs(fmid), na.rm = TRUE), "]\n", file = stderr())
        }

        if (all(abs(fmid) < tolerance)) break

        samesign.mid.lower <- ((fmid < 0) & (flower < 0)) | ((fmid >= 0) & (flower >= 0))
        lower <- ifelse(samesign.mid.lower, mid, lower)
        flower <- ifelse(samesign.mid.lower, fmid, flower)
        upper <- ifelse(!samesign.mid.lower, mid, upper)
        fupper <- ifelse(!samesign.mid.lower, fmid, fupper)
    }

    list(mid = mid, fmid = fmid, lower = lower, upper = upper,
         flower = flower, fupper = fupper, n = n)
}
