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
#'
#' # Area chart of cumulative portfolio returns over trading days
#' set.seed(42)
#' days <- 1:252
#' cum_ret <- cumsum(rnorm(252, mean = 0.0004, sd = 0.01))
#' iaw$plot.timearea(days, cum_ret, col = "lightblue",
#'                   main = "Cumulative daily returns (1 year)",
#'                   xlab = "Day", ylab = "Return")
#' lines(days, cum_ret, lwd = 1.5)
#'
#' # GDP quarterly time series as a filled area
#' quarters <- seq(2010, 2024, by = 0.25)
#' gdp <- 15000 + cumsum(rnorm(length(quarters), 50, 80))
#' iaw$plot.timearea(quarters, gdp,
#'                   col = adjustcolor("steelblue", alpha.f = 0.5),
#'                   main = "Quarterly GDP ($B)")
#' lines(quarters, gdp, col = "navy", lwd = 2)
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
