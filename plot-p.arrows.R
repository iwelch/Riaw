#' Plot Arrows
#'
#' @name plot.p.arrows
#'
#' Draws arrows on plot.
#'
#' @param x0 Start x.
#' @param y0 Start y.
#' @param x1 End x.
#' @param y1 End y.
#' @param ... Arrow arguments.
#'
#' @return Invisible NULL.
#'
#' @family plotting
#' @export

iaw$plot.p.arrows <- function(x0, y0, x1, y1, ...) {
    arrows(x0, y0, x1, y1, ...)
    invisible(NULL)
}
