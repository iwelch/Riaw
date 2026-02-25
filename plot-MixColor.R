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
#' @examples
#' # 50/50 mix of red and blue
#' iaw$mixcolor("red", "blue")
#'
#' # 25% red, 75% blue
#' iaw$mixcolor("red", "blue", ratio = 0.25)
#'
#' \dontrun{
#' # Use mixed colors in a gradient legend
#' cols <- sapply(seq(0, 1, by = 0.1), function(r) iaw$mixcolor("red", "blue", r))
#' barplot(rep(1, length(cols)), col = cols, border = NA,
#'         main = "Red-to-blue gradient")
#' }
#'
#' @family plotting
#' @export

iaw$mixcolor <- function(col1, col2, ratio = 0.5) {
    n <- 101
    colorRampPalette(c(col1, col2))(n)[round(ratio * (n - 1)) + 1]
}
