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
#' @family plotting
#' @export

iaw$plot.native.slope <- function(slope) {
    stopifnot(is.numeric(slope), length(slope)==1)
    usr <- par("usr")
    pin <- par("pin")
    asp <- diff(usr[3:4]) / diff(usr[1:2]) * pin[1] / pin[2]
    atan(slope * asp) * 180 / pi
}
