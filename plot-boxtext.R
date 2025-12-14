#' Text with Box Background
#'
#' @name plot.boxtext
#'
#' Adds text with background box.
#'
#' @param x X position.
#' @param y Y position.
#' @param labels Text labels.
#' @param ... Text arguments.
#'
#' @return Invisible NULL.
#'
#' @family plotting
#' @export

iaw$plot.boxtext <- function(x, y, labels, ...) {
    text(x, y, labels, ...)
    invisible(NULL)
}
