#' Rbind Results from mc.by
#'
#' @name rbind.mc.by
#'
#' Combines mc.by results into data frame.
#'
#' @param indata Data frame.
#' @param INDICES Grouping variable.
#' @param FUNIN Function returning data frames.
#' @param ... Additional arguments.
#'
#' @return Combined data frame.
#'
#' @family parallel
#' @export

iaw$rbind.mc.by <- function(indata, INDICES, FUNIN, ...) {
    stopifnot(is.data.frame(indata))
    result <- iaw$mc.by(indata, INDICES, FUNIN, ...)
    do.call(rbind, result)
}
