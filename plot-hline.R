#' Add Horizontal Reference Lines to a Plot
#'
#' Draws horizontal lines across the plot at specified y-coordinates. Useful for
#' adding reference lines at zero, means, or other significant values.
#'
#' @param atyloc Numeric vector of y-coordinates where horizontal lines should
#'   be drawn.
#' @param xrange Optional numeric vector of length 2 specifying the x-range
#'   \code{c(xmin, xmax)}. If NULL, extends to the plot boundaries.
#' @param ... Additional graphical parameters passed to \code{lines()}, such as
#'   \code{col}, \code{lty}, \code{lwd}.
#'
#' @return Invisibly returns NULL. Called for its side effect of adding lines
#'   to the current plot.
#'
#' @details
#' When \code{xrange} is not specified, the function automatically detects
#' log-scale axes and adjusts the line extent appropriately.
#'
#' @export
#'
#' @seealso \code{\link{iaw$vline}} for vertical lines, \code{\link{abline}}
#'
#' @examples
#' # Basic scatter plot with reference lines
#' x <- rnorm(100)
#' y <- x + rnorm(100)
#' plot(x, y)
#'
#' # Add horizontal line at y = 0
#' iaw$hline(0)
#'
#' # Add dashed line at mean of y
#' iaw$hline(mean(y), col = "blue", lty = 2)
#'
#' # Multiple reference lines
#' iaw$hline(c(-1, 0, 1), col = "gray", lty = 3)
#'
#' # Custom line style
#' iaw$hline(0, col = "red", lwd = 2, lty = 1)

iaw$hline <- function(atyloc, xrange = NULL, ...) {
    if (length(xrange) != 2) {
        xrange <- if (par("xlog")) c(9e-99, 9e99) else c(-9e99, 9e99)
    }

    for (i in seq_along(atyloc)) {
        lines(xrange, c(atyloc[i], atyloc[i]), ...)
    }

    invisible(NULL)
}
