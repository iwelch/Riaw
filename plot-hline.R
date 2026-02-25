#' Add Horizontal Lines to Plot
#'
#' @name hline
#'
#' Draws horizontal lines at specified y values.
#'
#' @param atyloc Y coordinates for lines.
#' @param xrange Optional x range.
#' @param ... Graphics parameters.
#'
#' @return Invisible NULL.
#'
#' @family plotting
#' @export
#'
#' @examples
#' \dontrun{
#' plot(1:10)
#' iaw$hline(5, col = "red")
#'
#' # Multiple reference lines at once
#' plot(rnorm(100))
#' iaw$hline(c(-2, 0, 2), col = "gray", lty = 2)
#'
#' # Mark a mean line on a histogram
#' x <- rnorm(200, mean = 5)
#' plot(density(x))
#' iaw$hline(0.1, col = "blue", lty = 3, lwd = 2)
#'
#' # Zero-line on a returns chart (standard finance convention)
#' set.seed(1)
#' returns <- rnorm(60, 0.001, 0.02)
#' plot(returns, type = "h", main = "Monthly excess returns")
#' iaw$hline(0, col = "black", lwd = 1.5)
#'
#' # +/- 1 standard-deviation bands around a mean
#' vals <- rnorm(200, mean = 10, sd = 2)
#' plot(vals, pch = 16, cex = 0.5)
#' iaw$hline(mean(vals), col = "blue", lwd = 2)
#' iaw$hline(mean(vals) + c(-1, 1) * sd(vals), col = "blue", lty = 2)
#' }

iaw$hline <- function(atyloc, xrange = NULL, ...) {
    stopifnot(is.numeric(atyloc))
    if (length(xrange) != 2) {
        xrange <- if (par("xlog")) c(9e-99, 9e99) else c(-9e99, 9e99)
    }
    for (i in seq_along(atyloc)) {
        lines(xrange, c(atyloc[i], atyloc[i]), ...)
    }
    invisible(NULL)
}
