#' Fast OLS Without NA Checking (Deprecated)
#'
#' @name ols.nona
#'
#' Deprecated. Use \code{RcppArmadillo::fastLm} instead.
#'
#' @param ... Ignored. Function always stops with deprecation message.
#'
#' @return Never returns; always stops with an error.
#'
#' @examples
#' \dontrun{
#' # ols.nona is deprecated; calling it always stops with a message
#' iaw$ols.nona()
#' # Use RcppArmadillo::fastLm instead:
#' # library(RcppArmadillo)
#' # fastLm(y ~ x, data = df)
#' }
#'
#' @family regression
#' @export

iaw$ols.nona <- function(...) {

    stop("please use library(RcppArmadillo); and then `fastLmPure(X, y)` or `fastLm(X, ...)` or `fastLm(formula, data)")

}
