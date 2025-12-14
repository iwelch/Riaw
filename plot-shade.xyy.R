#' Shade Area Between Lines
#'
#' @name plot.shade.xyy
#'
#' Shades area between two y values.
#'
#' @param x X coordinates.
#' @param y1 Lower y values.
#' @param y2 Upper y values.
#' @param col Fill color.
#' @param ... Polygon arguments.
#'
#' @return Invisible NULL.
#'
#' @family plotting
#' @export

iaw$plot.shade.xyy <- function(x, y1, y2, col = "lightgray", ...) {
    stopifnot(is.numeric(x), is.numeric(y1), is.numeric(y2))
    polygon(c(x, rev(x)), c(y1, rev(y2)), col = col, border = NA, ...)
    invisible(NULL)
}
