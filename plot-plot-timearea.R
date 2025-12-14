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
#' @family plotting
#' @export

iaw$plot.timearea <- function(x, y, ...) {
    stopifnot(is.numeric(x), is.numeric(y))
    plot(x, y, type = "n")
    polygon(c(x, rev(x)), c(y, rep(0, length(y))), ...)
    invisible(NULL)
}
