#' Wavelength to Color
#'
#' @name lambda2color
#'
#' Converts wavelength to RGB color.
#'
#' @param lambda Wavelength in nm.
#'
#' @return Color value.
#'
#' @examples
#' # Red light (~700 nm)
#' iaw$lambda2color(700)
#'
#' # Blue light (~450 nm)
#' iaw$lambda2color(450)
#'
#' # Green light (~550 nm)
#' iaw$lambda2color(550)
#'
#' # Boundary of visible spectrum (violet)
#' iaw$lambda2color(380)
#'
#' # Vectorise over a small set of wavelengths
#' sapply(c(400, 500, 600, 700), iaw$lambda2color)
#'
#' \dontrun{
#' # Use wavelength colors in a spectrum plot
#' lambdas <- seq(380, 780, by = 10)
#' cols <- sapply(lambdas, iaw$lambda2color)
#' barplot(rep(1, length(lambdas)), col = cols, border = NA,
#'         names.arg = lambdas, las = 2, main = "Visible spectrum")
#'
#' # Color-code data points by a wavelength-mapped variable
#' x <- seq(380, 780, length.out = 50)
#' y <- sin(seq(0, 4 * pi, length.out = 50))
#' plot(x, y, pch = 16, col = sapply(x, iaw$lambda2color),
#'      xlab = "Wavelength (nm)", ylab = "Signal", cex = 1.5)
#' }
#'
#' @family plotting
#' @export

iaw$lambda2color <- function(lambda) {
    stopifnot(is.numeric(lambda))
    rainbow(1, start = (lambda - 380) / 400)
}
