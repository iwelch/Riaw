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
#'
#' # Mark recession start dates on an economic time series
#' years <- 2000:2025
#' gdp_growth <- rnorm(length(years), 2, 1.5)
#' plot(years, gdp_growth, type = "l", lwd = 2, main = "GDP growth (%)")
#' abline(h = 0, lty = 3)
#' iaw$vline(c(2001, 2008, 2020), col = "gray70", lty = 2)
#' text(c(2001, 2008, 2020), rep(max(gdp_growth), 3),
#'      c("2001", "GFC", "COVID"), pos = 4, cex = 0.7)
#'
#' # Vertical line with restricted y-range (partial line)
#' plot(1:20, rnorm(20), ylim = c(-3, 3))
#' iaw$vline(10, yrange = c(-1, 1), col = "purple", lwd = 3)
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
