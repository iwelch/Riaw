#' Loess Smoothed Line Plot
#'
#' @name plot.loess.lineplot
#'
#' Plots loess-smoothed line.
#'
#' @param x X values.
#' @param y Y values.
#' @param span Smoothing span.
#' @param ... Plot arguments.
#'
#' @return Invisible NULL.
#'
#' @family plotting
#' @export

iaw$plot.loess.lineplot <- function(x, y, span = 0.75, ...) {
    stopifnot(is.numeric(x), is.numeric(y))
    lo <- loess(y ~ x, span = span)
    plot(x, y, type = "n", ...)
    ox <- order(x)
    lines(x[ox], predict(lo)[ox])
    invisible(NULL)
}
