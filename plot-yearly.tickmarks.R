#' Add Yearly Tickmarks
#'
#' @name plot.yearly.tickmarks
#'
#' Adds yearly tick marks to time axis.
#'
#' @param years Years to mark.
#' @param ... Axis arguments.
#'
#' @return Invisible NULL.
#'
#' @family plotting
#' @export

iaw$plot.yearly.tickmarks <- function(years, ...) {
    stopifnot(is.numeric(years))
    axis(1, at = years, labels = years, ...)
    invisible(NULL)
}
