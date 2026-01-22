#' Bisection Root Finding (Vectorized)
#'
#' @name bisection
#'
#' Finds roots using bisection method. Supports vectorized inputs.
#'
#' @param f Function to find root of.
#' @param lower Numeric vector of lower bounds.
#' @param upper Numeric vector of upper bounds.
#' @param ... Additional arguments passed to f.
#' @param numiter Maximum iterations. Default 100.
#' @param tolerance Convergence tolerance. Default .Machine$double.eps^0.25.
#' @param verbose Print progress. Default 0.
#'
#' @return List with mid, fmid, lower, upper, flower, fupper, n.
#'
#' @family utilities
#' @export
#'
#' @examples
#' iaw$bisection(function(x) x^2 - 2, 0, 2)

iaw$bisection <- function(f, lower, upper, ..., numiter = 100,
                          tolerance = .Machine$double.eps^0.25, verbose = 0) {
  ## input validation
  stopifnot(is.function(f))
  stopifnot(!is.null(lower), is.numeric(lower), length(lower) > 0)
  stopifnot(!is.null(upper), is.numeric(upper), length(upper) > 0)
  stopifnot(length(lower) == length(upper))
  stopifnot(is.numeric(numiter), length(numiter) == 1, numiter > 0)

  ## evaluate at bounds
  fl <- f(lower, ...)
  fu <- f(upper, ...)

  stopifnot(all(is.numeric(fl)), all(!is.na(fl)))
  stopifnot(all(is.numeric(fu)), all(!is.na(fu)))

  if (any(bad <- (fl * fu > 0))) {
    nbad <- sum(bad)
    idx <- head(which(bad), 3)
    stop(sprintf("%d/%d cases have same sign at bounds. First: %s (f(lower)=%s, f(upper)=%s)",
                 nbad, length(fl), paste(idx, collapse = ","),
                 paste(round(fl[idx], 4), collapse = ","),
                 paste(round(fu[idx], 4), collapse = ",")))
  }

  ## iterate
  for (n in seq_len(numiter)) {
    mid <- (lower + upper) / 2
    fm <- f(mid, ...)

    if (verbose) cat("[bisection:", n, "/", numiter,
                     " mean|f|=", mean(abs(fm), na.rm = TRUE),
                     " max|f|=", max(abs(fm), na.rm = TRUE), "]\n", file = stderr())

    if (all(abs(fm) < tolerance)) break

    same.sign.lower <- ((fm < 0) & (fl < 0)) | ((fm >= 0) & (fl >= 0))
    lower <- ifelse(same.sign.lower, mid, lower)
    fl    <- ifelse(same.sign.lower, fm, fl)
    upper <- ifelse(!same.sign.lower, mid, upper)
    fu    <- ifelse(!same.sign.lower, fm, fu)
  }

  list(mid = mid, fmid = fm, lower = lower, upper = upper,
       flower = fl, fupper = fu, n = n)
}
