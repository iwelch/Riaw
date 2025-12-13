
#' SDP
#'
#' @name sdp
#'
#' the population standard deviation
#'
#' @usage sdp(x, ...)
#'
#' @param x the numeric input vector
#'
#' @return a numeric population standard deviation
#'


iaw$sdp <- function (x, ...) return(sqrt(iaw$varp(x, ...)))
