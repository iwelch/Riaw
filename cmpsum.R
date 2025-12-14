#' Compound Sum
#'
#' @name cmpsum
#'
#' Calculates compound sum of returns.
#'
#' @param r Returns vector.
#'
#' @return Compound sum.
#'
#' @family statistics
#' @export
#'
#' @examples
#' iaw$cmpsum(c(0.1, 0.05, -0.02))

iaw$cmpsum <- function(r) {
    stopifnot(is.numeric(r))
    prod(1 + r) - 1
}
