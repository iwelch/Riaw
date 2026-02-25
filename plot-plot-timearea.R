#' Time Series Area Plot
#'
#' @name plot.timearea
#'
#' Creates time series area plot.
#'
#' @param x Time values.
#' @param y Y values.
#' @param ... Polygon arguments.
#'
#' @return Invisible NULL.
#'
#' @examples
#' \dontrun{
#' # Area chart for a simple time series
#' t <- 1:24
#' y <- c(2, 3, 4, 3, 5, 6, 5, 4, 6, 8, 7, 6,
#'        5, 4, 5, 6, 7, 8, 9, 8, 7, 6, 5, 4)
#' iaw$plot.timearea(t, y, col = "steelblue", main = "Monthly values")
#'
#' # Highlight positive area in green, overlap with a line
#' iaw$plot.timearea(t, y, col = adjustcolor("green", alpha.f = 0.4))
#' lines(t, y, lwd = 2)
#' }
#'
#' @family plotting
#' @export

iaw$plot.timearea <- function(x, y, ...) {
    stopifnot(is.numeric(x), is.numeric(y))
    plot(x, y, type = "n")
    polygon(c(x, rev(x)), c(y, rep(0, length(y))), ...)
    invisible(NULL)
}
