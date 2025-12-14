#' Add Horizontal Lines to Plot
#'
#' @name hline
#'
#' Draws horizontal lines at specified y values.
#'
#' @param atyloc Y coordinates for lines.
#' @param xrange Optional x range.
#' @param ... Graphics parameters.
#'
#' @return Invisible NULL.
#'
#' @family plotting
#' @export
#'
#' @examples
#' plot(1:10)
#' iaw$hline(5, col = "red")

iaw$hline <- function(atyloc, xrange = NULL, ...) {
    stopifnot(is.numeric(atyloc))
    if (length(xrange) != 2) {
        xrange <- if (par("xlog")) c(9e-99, 9e99) else c(-9e99, 9e99)
    }
    for (i in seq_along(atyloc)) {
        lines(xrange, c(atyloc[i], atyloc[i]), ...)
    }
    invisible(NULL)
}
