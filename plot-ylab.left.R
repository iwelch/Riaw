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
#' }
#'
#' @family plotting
#' @export

iaw$plot.ylab.left <- function(label, ...) {
    stopifnot(is.character(label))
    mtext(label, side = 2, line = 3, ...)
    invisible(NULL)
}
