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
#'
#' # Visualize a saddle-shaped surface (e.g., portfolio risk surface)
#' x2 <- seq(-2, 2, length.out = 40)
#' y2 <- seq(-2, 2, length.out = 40)
#' z2 <- outer(x2, y2, function(a, b) a^2 - b^2)
#' iaw$plot.contour(x2, y2, z2, main = "Saddle surface",
#'                  color.palette = terrain.colors)
#'
#' # Rosenbrock function (optimization benchmark)
#' x3 <- seq(-2, 2, length.out = 50)
#' y3 <- seq(-1, 3, length.out = 50)
#' z3 <- outer(x3, y3, function(a, b) (1 - a)^2 + 100 * (b - a^2)^2)
#' iaw$plot.contour(x3, y3, log(z3 + 1), main = "log Rosenbrock",
#'                  nlevels = 20)
#' }
#'
#' @family plotting
#' @export

iaw$plot.contour <- function(x, y, z, ...) {
    stopifnot(is.numeric(x), is.numeric(y), is.matrix(z))
    filled.contour(x, y, z, ...)
    invisible(NULL)
}
