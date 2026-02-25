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
#'
#' # Quarterly financial data with yearly labels on the x-axis
#' set.seed(5)
#' quarters <- seq(2015, 2025, by = 0.25)
#' revenue <- 100 + cumsum(rnorm(length(quarters), 1, 3))
#' plot(quarters, revenue, type = "l", xaxt = "n", lwd = 2,
#'      main = "Quarterly Revenue", xlab = "Year", ylab = "$M")
#' iaw$plot.yearly.tickmarks(2015:2025)
#'
#' # Rotated year labels for a long time span
#' plot(1900:2025, cumsum(rnorm(126)), type = "l", xaxt = "n",
#'      xlab = "Year", ylab = "Index")
#' iaw$plot.yearly.tickmarks(seq(1900, 2025, by = 25), las = 2)
#' }
#'
#' @family plotting
#' @export

iaw$plot.yearly.tickmarks <- function(years, ...) {
    stopifnot(is.numeric(years))
    axis(1, at = years, labels = years, ...)
    invisible(NULL)
}
