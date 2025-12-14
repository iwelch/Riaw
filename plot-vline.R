#' Add Vertical Reference Lines to a Plot
#'
#' Draws vertical lines across the plot at specified x-coordinates. Useful for
#' marking events, thresholds, or time points.
#'
#' @param atxloc Numeric vector of x-coordinates where vertical lines should
#'   be drawn.
#' @param yrange Optional numeric vector of length 2 specifying the y-range
#'   \code{c(ymin, ymax)}. If NULL, extends to the plot boundaries.
#' @param ... Additional graphical parameters passed to \code{lines()}, such as
#'   \code{col}, \code{lty}, \code{lwd}.
#'
#' @return Invisibly returns NULL. Called for its side effect of adding lines
#'   to the current plot.
#'
#' @details
#' When \code{yrange} is not specified, the function automatically detects
#' log-scale axes and adjusts the line extent appropriately.
#'
#' @export
#'
#' @seealso \code{\link{iaw$hline}} for horizontal lines, \code{\link{abline}}
#'
#' @examples
#' # Time series plot with event markers
#' dates <- 1:100
#' values <- cumsum(rnorm(100))
#' plot(dates, values, type = "l")
#'
#' # Mark specific dates
#' iaw$vline(c(25, 50, 75), col = "gray", lty = 2)
#'
#' # Highlight a particular point
#' iaw$vline(50, col = "red", lwd = 2)
#'
#' # With custom range
#' iaw$vline(25, yrange = c(-5, 5), col = "blue")

iaw$vline <- function(atxloc, yrange = NULL, ...) {
    if (length(yrange) != 2) {
        yrange <- if (par("ylog")) c(9e-99, 9e99) else c(-9e99, 9e99)
    }

    for (i in seq_along(atxloc)) {
        lines(c(atxloc[i], atxloc[i]), yrange, ...)
    }

    invisible(NULL)
}
