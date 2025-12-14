#' ECDF Line Plot
#'
#' @name plot.ecdf.lineplot
#'
#' Plots empirical CDF.
#'
#' @param x Numeric vector.
#' @param ... Plot arguments.
#'
#' @return Invisible NULL.
#'
#' @family plotting
#' @export

iaw$plot.ecdf.lineplot <- function(x, ...) {
    stopifnot(is.numeric(x))
    plot(ecdf(x), ...)
    invisible(NULL)
}
