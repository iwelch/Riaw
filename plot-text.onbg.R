#' Text on Background
#'
#' @name plot.text.onbg
#'
#' Alias for text with background.
#'
#' @param x X position.
#' @param y Y position.
#' @param labels Text.
#' @param ... Arguments.
#'
#' @return Invisible NULL.
#'
#' @examples
#' \dontrun{
#' # Alias for iaw$plot.text.bg - adds text with a colored background
#' plot(1:10, 1:10, type = "n")
#' iaw$plot.text.onbg(5, 5, "center", bg = "lightyellow")
#' }
#'
#' @family plotting
#' @keywords internal

iaw$plot.text.onbg <- iaw$plot.text.bg
