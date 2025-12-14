#' 2D Histogram
#'
#' @name plot.hist2d
#'
#' Creates 2D histogram/heatmap.
#'
#' @param x X values.
#' @param y Y values.
#' @param nbins Number of bins.
#'
#' @return Invisible NULL.
#'
#' @family plotting
#' @export

iaw$plot.hist2d <- function(x, y, nbins = 50) {
    stopifnot(is.numeric(x), is.numeric(y))
    stopifnot(length(x) == length(y))
    h <- MASS::kde2d(x, y, n = nbins)
    filled.contour(h)
    invisible(NULL)
}
