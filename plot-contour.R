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
#' @examples
#' \dontrun{
#' # Simple filled contour of a bivariate normal density surface
#' x <- seq(-3, 3, length.out = 30)
#' y <- seq(-3, 3, length.out = 30)
#' z <- outer(x, y, function(a, b) exp(-0.5 * (a^2 + b^2)))
#' iaw$plot.contour(x, y, z)
#'
#' # Add a color palette and axis labels
#' iaw$plot.contour(x, y, z,
#'                  color.palette = heat.colors,
#'                  xlab = "X", ylab = "Y",
#'                  main = "Bivariate density")
#' }
#'
#' @family plotting
#' @export

iaw$plot.contour <- function(x, y, z, ...) {
    stopifnot(is.numeric(x), is.numeric(y), is.matrix(z))
    filled.contour(x, y, z, ...)
    invisible(NULL)
}
