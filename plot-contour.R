#' Contour Plot
#'
#' @name plot.contour
#'
#' Creates filled contour plot.
#'
#' @param x X coordinates.
#' @param y Y coordinates.
#' @param z Matrix of z values.
#' @param ... Additional arguments.
#'
#' @return Invisible NULL.
#'
#' @family plotting
#' @export

iaw$plot.contour <- function(x, y, z, ...) {
    stopifnot(is.numeric(x), is.numeric(y), is.matrix(z))
    filled.contour(x, y, z, ...)
    invisible(NULL)
}
