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
#' @examples
#' \dontrun{
#' # 2D histogram of bivariate normal data
#' set.seed(1)
#' x <- rnorm(1000)
#' y <- rnorm(1000)
#' iaw$plot.hist2d(x, y)
#'
#' # Correlated variables with finer binning
#' set.seed(2)
#' x2 <- rnorm(2000)
#' y2 <- 0.8 * x2 + rnorm(2000, sd = 0.6)
#' iaw$plot.hist2d(x2, y2, nbins = 80)
#' }
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
