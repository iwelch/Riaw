
#' MA
#'
#' @name ma
#'
#' the moving average of a vector
#' @usage ma (x, n = 5)
#' @param x vector
#' @param n number of elements to use in average
#' @return vector of moving average
#'

iaw$ma <- function (x, n = 5, around=FALSE) (filter(x, rep(1/n, n), sides = if (around) 2 else 1))
