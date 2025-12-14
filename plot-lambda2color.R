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
#' @family plotting
#' @export

iaw$lambda2color <- function(lambda) {
    stopifnot(is.numeric(lambda))
    rainbow(1, start = (lambda - 380) / 400)
}
