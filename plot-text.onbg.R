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
#'
#' # Label multiple points on a scatter plot
#' set.seed(1)
#' x <- rnorm(20); y <- rnorm(20)
#' plot(x, y, pch = 16)
#' iaw$plot.text.onbg(x[1], y[1], "A", bg = "white", col = "red")
#' iaw$plot.text.onbg(x[2], y[2], "B", bg = "white", col = "blue")
#'
#' # Annotate a key threshold on a time-series chart
#' ts_val <- cumsum(rnorm(100))
#' plot(ts_val, type = "l", main = "Performance")
#' threshold_idx <- which.max(ts_val)
#' iaw$plot.text.onbg(threshold_idx, ts_val[threshold_idx],
#'                    "Peak", bg = "lightyellow", col = "darkred")
#' }
#'
#' @family plotting
#' @keywords internal

iaw$plot.text.onbg <- iaw$plot.text.bg
