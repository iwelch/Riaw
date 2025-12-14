#' Plot Points with Subsampling
#'
#' @name plot.points.toomany
#'
#' Plots subsampled points for large datasets.
#'
#' @param x X values.
#' @param y Y values.
#' @param maxpoints Maximum points to plot.
#' @param ... Plot arguments.
#'
#' @return Invisible NULL.
#'
#' @family plotting
#' @export

iaw$plot.points.toomany <- function(x, y, maxpoints = 10000, ...) {
    stopifnot(is.numeric(x), is.numeric(y))
    n <- length(x)
    if (n > maxpoints) {
        idx <- sample(n, maxpoints)
        x <- x[idx]
        y <- y[idx]
    }
    points(x, y, ...)
    invisible(NULL)
}
