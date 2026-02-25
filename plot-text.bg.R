#' Text with Background
#'
#' @name plot.text.bg
#'
#' Adds text with background color.
#'
#' @param x X position.
#' @param y Y position.
#' @param labels Text.
#' @param bg Background color.
#' @param ... Text arguments.
#'
#' @return Invisible NULL.
#'
#' @examples
#' \dontrun{
#' # Add a label with white background so it stands out on a busy plot
#' plot(rnorm(200), rnorm(200), pch = 16, col = "gray60")
#' iaw$plot.text.bg(0, 0, "Origin", bg = "white")
#'
#' # Yellow background annotation at a specific data point
#' x <- 1:10
#' y <- x^2
#' plot(x, y, type = "b")
#' iaw$plot.text.bg(5, 25, "peak zone", bg = "yellow", col = "darkred", font = 2)
#'
#' # Label a regression line on a busy scatter plot
#' set.seed(8)
#' x <- rnorm(300); y <- 0.6 * x + rnorm(300, sd = 0.8)
#' plot(x, y, pch = 16, col = "gray60", main = "Labeled regression")
#' abline(lm(y ~ x), col = "red", lwd = 2)
#' iaw$plot.text.bg(1.5, 0.6 * 1.5, "y = 0.6x", bg = "white", col = "red")
#'
#' # Annotate an outlier in financial return data
#' returns <- c(rnorm(49, 0, 0.02), 0.15)
#' plot(seq_along(returns), returns, pch = 16, xlab = "Day", ylab = "Return")
#' iaw$plot.text.bg(50, 0.15, "Flash rally", bg = "lightyellow", col = "red", cex = 0.9)
#' }
#'
#' @family plotting
#' @export

iaw$plot.text.bg <- function(x, y, labels, bg = "white", ...) {
    rect(x - 0.1, y - 0.1, x + 0.1, y + 0.1, col = bg, border = NA)
    text(x, y, labels, ...)
    invisible(NULL)
}
