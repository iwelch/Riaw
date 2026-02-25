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
#'
#' # Highlight periods where a fund outperforms its benchmark
#' set.seed(10)
#' months <- 1:60
#' fund  <- cumsum(rnorm(60, 0.005, 0.03))
#' bench <- cumsum(rnorm(60, 0.004, 0.02))
#' plot(months, fund, type = "n", ylim = range(c(fund, bench)),
#'      main = "Fund vs Benchmark", xlab = "Month", ylab = "Cumulative return")
#' iaw$plot.shade.xyy(months, fund, bench,
#'                    col.plus = adjustcolor("green", 0.4),
#'                    col.minus = adjustcolor("red", 0.4))
#' lines(months, fund, col = "darkgreen", lwd = 2)
#' lines(months, bench, col = "darkred", lwd = 2)
#' legend("topleft", c("Fund", "Benchmark"), col = c("darkgreen", "darkred"), lwd = 2)
#'
#' # Shade temperature difference between two cities
#' days <- 1:365
#' temp_a <- 15 + 10 * sin(2 * pi * days / 365) + rnorm(365, 0, 2)
#' temp_b <- 12 + 8 * sin(2 * pi * days / 365 + 0.5) + rnorm(365, 0, 2)
#' plot(days, temp_a, type = "n", ylim = range(c(temp_a, temp_b)),
#'      xlab = "Day of year", ylab = "Temp (C)", main = "City A vs City B")
#' iaw$plot.shade.xyy(days, temp_a, temp_b,
#'                    col.plus = "coral", col.minus = "skyblue")
#' lines(days, temp_a, col = "red"); lines(days, temp_b, col = "blue")
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
