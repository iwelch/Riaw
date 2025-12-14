#' Mix Colors
#'
#' @name MixColor
#'
#' Mixes two colors.
#'
#' @param col1 First color.
#' @param col2 Second color.
#' @param ratio Mix ratio.
#'
#' @return Color value.
#'
#' @family plotting
#' @export

iaw$mixcolor <- function(col1, col2, ratio = 0.5) {
    colorRampPalette(c(col1, col2))(3)[2]
}
