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
#'
#' # Visualize size-value factor joint distribution
#' set.seed(10)
#' size_factor <- rlnorm(3000, meanlog = 0, sdlog = 0.8)
#' value_factor <- 0.3 * log(size_factor) + rnorm(3000, sd = 0.5)
#' iaw$plot.hist2d(log(size_factor), value_factor, nbins = 60)
#'
#' # Sparse data: coarser bins reveal structure better
#' set.seed(5)
#' x3 <- rnorm(200)
#' y3 <- x3^2 + rnorm(200, sd = 0.5)
#' iaw$plot.hist2d(x3, y3, nbins = 25)
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
