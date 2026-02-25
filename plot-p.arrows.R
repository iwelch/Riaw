#' Plot Arrows
#'
#' @name plot.p.arrows
#'
#' Draws arrows on plot.
#'
#' @param x0 Start x.
#' @param y0 Start y.
#' @param x1 End x.
#' @param y1 End y.
#' @param ... Arrow arguments.
#'
#' @return Invisible NULL.
#'
#' @examples
#' \dontrun{
#' # Deprecated: use Arrows() from the 'shape' package directly
#' # iaw$p.arrows() calls .Defunct() and will error; shown for documentation only
#' library(shape)
#' plot(1:10, 1:10, type = "n")
#' Arrows(2, 2, 8, 8, arr.type = "triangle")
#'
#' # Multiple arrows showing fund-flow directions
#' plot(1:10, 1:10, type = "n", main = "Capital flow diagram")
#' shape::Arrows(2, 5, 8, 8, arr.type = "triangle", col = "blue")
#' shape::Arrows(2, 5, 8, 2, arr.type = "triangle", col = "red")
#' text(c(8, 8), c(8, 2), c("Equities", "Bonds"), pos = 4)
#' }
#'
#' @family plotting
#' @export

iaw$p.arrows <- function(x0, y0, x1, y1, ...) {
    .Defunct("use Arrows() from the shape library")

    shape::arrows(x0, y0, x1, y1, ...)
    invisible(NULL)
}
