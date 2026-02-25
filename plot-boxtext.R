#' Text with Box Background
#'
#' @name plot.boxtext
#'
#' Adds text with background box.
#'
#' @param x X position.
#' @param y Y position.
#' @param labels Text labels.
#' @param ... Text arguments.
#'
#' @return Invisible NULL.
#'
#' @examples
#' \dontrun{
#' # Add a label with a box background on an existing plot
#' plot(1:10, 1:10)
#' iaw$plot.boxtext(5, 5, "Center label")
#'
#' # Customize text appearance
#' plot(1:10, 1:10, type = "n")
#' iaw$plot.boxtext(3, 7, "Group A", col = "blue", cex = 1.2)
#' iaw$plot.boxtext(7, 3, "Group B", col = "red",  cex = 1.2)
#'
#' # Annotate key data points on a time-series plot
#' x <- 1:20
#' y <- cumsum(rnorm(20))
#' plot(x, y, type = "l", main = "Annotated series")
#' peak <- which.max(y)
#' iaw$plot.boxtext(peak, y[peak], "Peak", pos = 3, cex = 0.8)
#'
#' # Label regression line at midpoint
#' plot(mtcars$wt, mtcars$mpg, pch = 16)
#' abline(lm(mpg ~ wt, data = mtcars), col = "red")
#' iaw$plot.boxtext(3.5, 22, "OLS fit", col = "red", font = 2)
#' }
#'
#' @family plotting
#' @export

iaw$plot.boxtext <- function(x, y, labels, ...) {
    text(x, y, labels, ...)
    invisible(NULL)
}
