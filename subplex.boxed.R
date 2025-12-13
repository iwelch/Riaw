
#' SUBPLEX.BOXED
#'
#' @name subplex.boxed
#'
#'   a very slow but very safe unidimensional minimizer, which allows the user to give
#'   lower and upper values acceptable for a solution.
#'
#' @usage subplex.boxed (par, fn, control = list(), hessian = FALSE, lower, upper, ...)
#'
#' @param par starting value
#' @param fn function to minimize
#'
#' @return a value
#'
#'  @examples
#' 	subplex.test <- function (x, a = 5)
#' 	  sum(x + (x - a)^2 * (x - 2)^2)


iaw$subplex.boxed <- function(par, fn, control = list(), hessian = FALSE, lower, upper, ...,
                              optimizer=subplex) {
    stop("no longer works")

  box.transform <- function(variable, lower, upper) (atan(variable)/pi + 0.5) * (upper - lower) + lower
  unbox.transform <- function(variable, lower, upper) tan((((variable - lower)/(upper - lower)) - 0.5) * pi)
  boxedfn <- function(x, ...) fn(box.transform(x, lower, upper), ...)
  result <- optimizer(par = unbox.transform(par, lower, upper), fn = boxedfn, control = control, hessian = hessian, ...)
  result$par <- box.transform(result$par, lower, upper)
  result
}
