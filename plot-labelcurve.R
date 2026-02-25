#' Place a text label on a line, based on slope at an x-point
#'
#' @name plot.labelcurve
#'
#' places text label on slope at an x-point
#'
#' @param xseries X values.
#' @param yseries Y values.
#' @param xval the x-location for the label
#' @param label the text content of the label
#' @param ... Plot arguments.
#'
#' @return Invisible NULL.
#'
#' @examples
#' \dontrun{
#' # Label a sine curve at x = pi/2
#' x <- seq(0, 2 * pi, length.out = 100)
#' y <- sin(x)
#' plot(x, y, type = "l", main = "Labeled curve")
#' iaw$plot.labelcurve(x, y, xval = pi / 2, label = "sin(x)")
#'
#' # Label two curves at different x positions
#' plot(x, sin(x), type = "l", ylim = c(-1.5, 1.5))
#' lines(x, cos(x), col = "blue")
#' iaw$plot.labelcurve(x, sin(x),  xval = 1.0,      label = "sin", col = "black")
#' iaw$plot.labelcurve(x, cos(x),  xval = pi / 4,   label = "cos", col = "blue")
#'
#' # Label cumulative return series for two portfolios
#' t <- 1:250
#' growth <- cumsum(rnorm(250, 0.0004, 0.01))
#' value  <- cumsum(rnorm(250, 0.0003, 0.008))
#' plot(t, growth, type = "l", col = "darkgreen", ylim = range(c(growth, value)),
#'      xlab = "Trading day", ylab = "Cumulative return")
#' lines(t, value, col = "darkred")
#' iaw$plot.labelcurve(t, growth, xval = 180, label = "Growth", col = "darkgreen")
#' iaw$plot.labelcurve(t, value,  xval = 180, label = "Value",  col = "darkred")
#'
#' # Label a yield curve at a specific maturity
#' maturities <- c(0.25, 0.5, 1, 2, 3, 5, 7, 10, 20, 30)
#' yields <- c(5.2, 5.0, 4.7, 4.3, 4.1, 4.0, 4.1, 4.2, 4.5, 4.6)
#' plot(maturities, yields, type = "l", xlab = "Maturity (yr)", ylab = "Yield (%)")
#' iaw$plot.labelcurve(maturities, yields, xval = 5, label = "Belly")
#' }
#'
#' @family plotting
#' @export


iaw$plot.labelcurve <- function(xseries, yseries, xval, label, ...) {
  ## Place text label along a curve with rotation matching the local slope
  ## Label is placed at the extrapolated position from neighboring points

  stopifnot(length(xseries) == length(yseries))
  stopifnot(length(xseries) >= 2)

  ## Find observation closest to xval
  idx <- which.min(abs(xseries - xval))

  ## Get the two points used for slope/extrapolation
  n <- length(xseries)
  if (idx == 1) {
    i1 <- 1
    i2 <- 2
  } else if (idx == n) {
    i1 <- n - 1
    i2 <- n
  } else {
    i1 <- idx - 1
    i2 <- idx + 1
  }

  x1 <- xseries[i1]
  y1 <- yseries[i1]
  x2 <- xseries[i2]
  y2 <- yseries[i2]

  ## Compute slope
  slope <- (y2 - y1) / (x2 - x1)

  ## Extrapolate y at xval
  y0 <- y1 + slope * (xval - x1)

  ## Convert data slope to visual angle
  ## Need to account for different axis scales and plot aspect ratio
  usr <- par("usr")  # (x1, x2, y1, y2)
  pin <- par("pin")  # (width, height) in inches

  x_range <- usr[2] - usr[1]
  y_range <- usr[4] - usr[3]

  ## slope in data units -> slope in inches/inch
  visual_slope <- slope * (pin[2] / y_range) / (pin[1] / x_range)

  ## convert to degrees for srt
  srt <- atan(visual_slope) * 180 / pi

  text(xval, y0, label, srt = srt, ...)
}
