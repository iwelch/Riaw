
#' Epoch to an R internal Object
#'
#' @name epoch2POSIXct
#'
#' convert the unix epoch time into something useful for R
#'
#' @usage epoch2POSIXct (unixepochtime)
#'
#' @param unixepochtime a large integer, like 15503800000
#'
#' @return an R internal object (which prints as a nice string), strictly a POSIX time
#'

iaw$epoch2POSIXct <- function (unixepochtime) structure(unixepochtime, class = c("POSIXt", "POSIXct"))
