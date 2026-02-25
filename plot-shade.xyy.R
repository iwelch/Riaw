#' Shade Area Between Two Lines with Directional Coloring
#'
#' @name plot.shade.xyy
#'
#' Shades area between ylo and yhi segment-by-segment, coloring
#' differently depending on which line is above.
#'
#' @param x X coordinates.
#' @param ylo First y series.
#' @param yhi Second y series.
#' @param col.plus Color when ylo > yhi. Default "blue".
#' @param col.minus Color when ylo < yhi. Default "red".
#' @param col.indif Color when crossing (currently unused).
#'
#' @return Invisible NULL.
#'
#' @examples
#' \dontrun{
#' # Shade between two sinusoids: blue where ylo > yhi, red where ylo < yhi
#' x    <- seq(0, 4 * pi, length.out = 200)
#' ylo  <- sin(x)
#' yhi  <- cos(x)
#' plot(x, ylo, type = "n", ylim = c(-1.5, 1.5),
#'      xlab = "x", ylab = "y", main = "Shaded difference")
#' iaw$plot.shade.xyy(x, ylo, yhi)
#' lines(x, ylo, lwd = 2)
#' lines(x, yhi, lwd = 2, lty = 2)
#' legend("topright", c("sin > cos", "cos > sin"),
#'        fill = c("blue", "red"))
#'
#' # Shade between a series and zero
#' x2   <- 1:50
#' y2   <- cumsum(rnorm(50))
#' plot(x2, y2, type = "l", main = "Cumulative sum vs. zero")
#' iaw$plot.shade.xyy(x2, ylo = y2, yhi = 0,
#'                    col.plus = "blue", col.minus = "red")
#' abline(h = 0, lty = 2)
#' }
#'
#' @family plotting
#' @export

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
    ## crossing segments (ylo and yhi swap sides) are left unshaded
  }
  invisible(NULL)
}
