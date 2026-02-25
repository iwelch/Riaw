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
#' @examples
#' \dontrun{
#' # Draw a time series and add yearly tick marks
#' plot(2000:2010, cumsum(rnorm(11)), type = "l", xaxt = "n")
#' iaw$plot.yearly.tickmarks(2000:2010)
#'
#' # Add tick marks for every other year with smaller text
#' plot(2000:2020, cumsum(rnorm(21)), type = "l", xaxt = "n")
#' iaw$plot.yearly.tickmarks(seq(2000, 2020, by = 2), cex.axis = 0.8)
#' }
#'
#' @family plotting
#' @export

iaw$plot.yearly.tickmarks <- function(years, ...) {
    stopifnot(is.numeric(years))
    axis(1, at = years, labels = years, ...)
    invisible(NULL)
}
