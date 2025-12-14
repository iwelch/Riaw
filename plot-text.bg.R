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
#' @family plotting
#' @export

iaw$plot.text.bg <- function(x, y, labels, bg = "white", ...) {
    rect(x - 0.1, y - 0.1, x + 0.1, y + 0.1, col = bg, border = NA)
    text(x, y, labels, ...)
    invisible(NULL)
}
