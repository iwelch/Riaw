#' One-Core By (Serial)
#'
#' @name oc.by
#'
#' Serial version of mc.by.
#'
#' @param indata Data frame.
#' @param INDICES Grouping variable.
#' @param FUNIN Function to apply.
#' @param ... Additional arguments.
#'
#' @return List of results.
#'
#' @family parallel
#' @export

iaw$oc.by <- function(indata, INDICES, FUNIN, ...) {
    stopifnot(is.data.frame(indata))
    ssplit <- split(seq_len(nrow(indata)), INDICES)
    lapply(ssplit, function(.index) {
        FUNIN(indata[.index, , drop = FALSE], ...)
    })
}
