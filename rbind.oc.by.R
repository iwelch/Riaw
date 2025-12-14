#' Rbind Results from oc.by
#'
#' @name rbind.oc.by
#'
#' Combines oc.by results into data frame.
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

iaw$rbind.oc.by <- function(indata, INDICES, FUNIN, ...) {
    stopifnot(is.data.frame(indata))
    result <- iaw$oc.by(indata, INDICES, FUNIN, ...)
    do.call(rbind, result)
}
