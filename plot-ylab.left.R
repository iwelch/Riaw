#' Add Left Y-axis Label
#'
#' @name plot.ylab.left
#'
#' Adds rotated y-axis label on left.
#'
#' @param label Label text.
#' @param ... Mtext arguments.
#'
#' @return Invisible NULL.
#'
#' @examples
#' \dontrun{
#' # Add a y-axis label after creating a plot with no default label
#' plot(1:10, rnorm(10), yaxt = "n", ylab = "")
#' axis(2)
#' iaw$plot.ylab.left("Return (%)")
#'
#' # Adjust the label font size
#' plot(1:10, rnorm(10), ylab = "")
#' iaw$plot.ylab.left("Cumulative return", cex = 1.2)
#'
#' # Add a y-axis label to a multi-panel layout
#' par(mfrow = c(2, 1), mar = c(3, 5, 2, 1))
#' plot(1:50, cumsum(rnorm(50)), type = "l", ylab = "")
#' iaw$plot.ylab.left("Panel A: Price ($)")
#' plot(1:50, rnorm(50, sd = 0.02), type = "h", ylab = "")
#' iaw$plot.ylab.left("Panel B: Daily return")
#' par(mfrow = c(1, 1))
#'
#' # Bold, colored y-axis label for emphasis
#' plot(rnorm(100), type = "l", ylab = "")
#' iaw$plot.ylab.left("Volatility (%)", font = 2, col = "darkred")
#' }
#'
#' @family plotting
#' @export

iaw$plot.ylab.left <- function(label, ...) {
    stopifnot(is.character(label))
    mtext(label, side = 2, line = 3, ...)
    invisible(NULL)
}
