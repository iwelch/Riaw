#' Add Vertical Lines to Plot
#'
#' @name vline
#'
#' Draws vertical lines at specified x values.
#'
#' @param atxloc X coordinates for lines.
#' @param yrange Optional y range.
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
#' iaw$vline(5, col = "blue")
#'
#' # Mark multiple event dates on a time-series plot
#' plot(1:100, cumsum(rnorm(100)), type = "l")
#' iaw$vline(c(25, 50, 75), col = "red", lty = 2)
#'
#' # Mark the mean of x on a scatter plot
#' x <- rnorm(50)
#' plot(x, rnorm(50))
#' iaw$vline(mean(x), col = "darkgreen", lwd = 2)
#' }

iaw$vline <- function(atxloc, yrange = NULL, ...) {
    stopifnot(is.numeric(atxloc))
    if (length(yrange) != 2) {
        yrange <- if (par("ylog")) c(9e-99, 9e99) else c(-9e99, 9e99)
    }
    for (i in seq_along(atxloc)) {
        lines(c(atxloc[i], atxloc[i]), yrange, ...)
    }
    invisible(NULL)
}
