
#' Population Variance
#'
#' @name varp
#'
#' population variance
#'
#' @usage varp (x, ...)
#'
#' @param a vector v
#'
#' @return
#'
#'
#'


iaw$varp <- function (x, ...) {
  lx <- if (is.data.frame(x) | is.matrix(x)) nrow(x) else length(x)
  (lx - 1)/lx * var(x, ...)
}
