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
#' # Pure endpoints: ratio 0 = col1, ratio 1 = col2
#' iaw$mixcolor("red", "blue", ratio = 0)  # "#FF0000" (pure red)
#' iaw$mixcolor("red", "blue", ratio = 1)  # "#0000FF" (pure blue)
#'
#' # Mix black and white to produce a medium gray
#' iaw$mixcolor("black", "white", ratio = 0.5)  # "#808080"
#'
#' # Create a custom heatmap color for a correlation value
#' r <- 0.73
#' col <- iaw$mixcolor("white", "darkred", ratio = abs(r))  # reddish for strong +r
#' nchar(col) > 0  # TRUE
#'
#' \dontrun{
#' # Use mixed colors in a gradient legend
#' cols <- sapply(seq(0, 1, by = 0.1), function(r) iaw$mixcolor("red", "blue", r))
#' barplot(rep(1, length(cols)), col = cols, border = NA,
#'         main = "Red-to-blue gradient")
#'
#' # Color bars by profit/loss magnitude using green-red mixing
#' returns <- c(0.05, -0.02, 0.10, -0.08, 0.03)
#' scaled <- (returns - min(returns)) / diff(range(returns))
#' bar_cols <- sapply(scaled, function(r) iaw$mixcolor("red", "forestgreen", r))
#' barplot(returns, col = bar_cols, names.arg = paste0("Q", 1:5),
#'         main = "Quarterly returns")
#' }
#'
#' @family plotting
#' @export

iaw$mixcolor <- function(col1, col2, ratio = 0.5) {
    n <- 101
    colorRampPalette(c(col1, col2))(n)[round(ratio * (n - 1)) + 1]
}
