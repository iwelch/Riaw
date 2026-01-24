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
