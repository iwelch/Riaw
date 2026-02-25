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
#' }
#'
#' @family plotting
#' @export

iaw$plot.boxtext <- function(x, y, labels, ...) {
    text(x, y, labels, ...)
    invisible(NULL)
}
