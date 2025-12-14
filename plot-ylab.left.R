#' Add Left Y-axis Label
#'
#' @name plot.ylab.left
#'
#' Adds rotated y-axis label on left.
#'
#' @param label Label text.
#' @param ... Mtext arguments.
#'
#' @return Invisible NULL.
#'
#' @family plotting
#' @export

iaw$plot.ylab.left <- function(label, ...) {
    stopifnot(is.character(label))
    mtext(label, side = 2, line = 3, ...)
    invisible(NULL)
}
