
#' COVP
#'
#' @name covp
#'
#' the population covariance of two vectors
#'
#' @usage covp (x, y, ...)
#'
#' @param x the first data vector
#' @param x the second data vector
#'
#' @return a scalar
#'


iaw$covp <- function (x, y, ...)  {
  lx <- if (is.data.frame(x) | is.matrix(x)) nrow(x) else length(x)
  ly <- if (is.data.frame(y) | is.matrix(y)) nrow(y) else length(y)
  (lx==ly) %or% "covariance must be on same number of observations"
  (lx - 1)/lx * cov(x, y, ...)
}
