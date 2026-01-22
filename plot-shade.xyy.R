#' Shade Area Between Lines
#'
#' @name plot.shade.xyy
#'
#' Shades area between two y values.
#'
#' @param x X coordinates.
#' @param y1 Lower y values.
#' @param y2 Upper y values.
#' @param col Fill color.
#' @param ... Polygon arguments.
#'
#' @return Invisible NULL.
#'
#' @family plotting
#' @export

if (0) {

iaw$plot.shade.xyy <- function(x, y1, y2, col = "lightgray", ...) {
    stopifnot(is.numeric(x), is.numeric(y1), is.numeric(y2))
    polygon(c(x, rev(x)), c(y1, rev(y2)), col = col, border = NA, ...)
    invisible(NULL)
}

} else {

iaw$plot.shade.xyy <- function (x, ylo, yhi, col.plus = "blue", col.minus = "red", col.indif = "gray")
{
  n <- length(x)
  if (length(ylo)==1) ylo <- rep(ylo, n)
  if (length(yhi)==1) yhi <- rep(yhi, n)
  stopifnot(all(!is.na(x)))
  stopifnot(all(!is.na(ylo)))
  stopifnot(all(!is.na(yhi)))

  for (i in 1:(n - 1)) {
    if ((ylo[i] < yhi[i]) && (ylo[i + 1] < yhi[i + 1])) {
      polygon(c(x[i], x[i + 1], x[i + 1], x[i]),
              c(ylo[i], ylo[i + 1], yhi[i + 1], yhi[i]), col = col.minus,
              border = FALSE)
    }
    else if ((ylo[i] > yhi[i]) && (ylo[i + 1] > yhi[i + 1])) {
      polygon(c(x[i], x[i + 1], x[i + 1], x[i]),
              c(ylo[i], ylo[i + 1], yhi[i + 1], yhi[i]), col = col.plus,
              border = FALSE)
    }
    else {
#      polygon(c(x[i], x[i + 1], x[i + 1], x[i]),
#              c(ylo[i], ylo[i + 1], yhi[i + 1], yhi[i]), col = col.indif,
#              border = FALSE)
    }
  }
}
}
