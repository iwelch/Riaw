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
#' }
#'
#' @family plotting
#' @export

iaw$plot.text.bg <- function(x, y, labels, bg = "white", ...) {
    rect(x - 0.1, y - 0.1, x + 0.1, y + 0.1, col = bg, border = NA)
    text(x, y, labels, ...)
    invisible(NULL)
}
