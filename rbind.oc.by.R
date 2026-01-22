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
    result <- Filter(Negate(is.null), result)
    if (length(result) == 0L) return(indata[0L, , drop = FALSE])
    out <- data.table::rbindlist(result)
    if (!inherits(indata, "data.table")) out <- as.data.frame(out)
    out
}
