#' Calculate Native Slope Angle
#'
#' @name plot.native.slope
#'
#' Calculates rotation angle for text along slope.
#'
#' @param slope Slope value.
#'
#' @return Angle in degrees.
#'
#' @examples
#' \dontrun{
#' # Get the visual rotation angle for a slope of 1 (45 degrees on a square plot)
#' plot(1:10, 1:10)
#' angle <- iaw$native.slope(1)
#' text(5, 5, "slope=1", srt = angle)
#'
#' # Label a regression line at the correct visual angle
#' x <- 1:20
#' y <- 0.5 * x + rnorm(20)
#' fit <- lm(y ~ x)
#' plot(x, y)
#' abline(fit)
#' srt <- iaw$native.slope(coef(fit)[["x"]])
#' text(10, predict(fit, data.frame(x = 10)), "fit", srt = srt, pos = 3)
#' }
#'
#' @family plotting
#' @export

iaw$native.slope <- function(slope) {
    stopifnot(is.numeric(slope), length(slope)==1)
    usr <- par("usr")
    pin <- par("pin")
    asp <- diff(usr[1:2]) / diff(usr[3:4]) * pin[2] / pin[1]
    atan(slope * asp) * 180 / pi
}
