
#' NEAREST
#'
#' @name nearest
#'
#'   find the value in a column of data that is closest to your own number.
#'
#' @usage nearest (col, your.number)
#'
#' @param nvec the numeric column
#' @param number the number to come close to
#'
#' @return the index value which is nearest
#'
#' @seealso which.nearest

iaw$nearest <- function( nvec, your.number ) nvec[ first(iaw$which.nearest( nvec, your.number )) ]
