#' Fast OLS Without NA Checking
#'
#' @name ols.nona
#'
#' Deprecated
#'

iaw$ols.nona <- function(...) {

    stop("please use library(RcppArmadillo); and then `fastLmPure(X, y)` or `fastLm(X, ...)` or `fastLm(formula, data)")

}
