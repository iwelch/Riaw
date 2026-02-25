#' Colorblind-Friendly Palette
#'
#' @name colorblind
#'
#' Returns colors suitable for colorblind viewers.
#'
#' @param n Number of colors.
#'
#' @return Character vector of colors.
#'
#' @family plotting
#' @export
#'
#' @examples
#' iaw$colorblind(5)    # "black" "orange" "skyblue" "green" "yellow"
#'
#' # Default: all 8 colors
#' iaw$colorblind()
#'
#' # Use in a base R plot with multiple groups
#' \dontrun{
#' cols <- iaw$colorblind(3)
#' plot(1:10, col = cols[1], pch = 16, ylim = c(0, 15))
#' points(1:10 + 2, col = cols[2], pch = 16)
#' points(1:10 + 4, col = cols[3], pch = 16)
#' }
#'
#' # Recycled when n > 8
#' iaw$colorblind(10)   # wraps back to "black", "orange", ...

iaw$colorblind <- function(n = 8) {
    stopifnot(is.numeric(n), length(n) == 1L, n > 0)
    palette <- c("black", "orange", "skyblue", "green", 
                 "yellow", "blue", "red", "gray")
    rep(palette, length.out = n)
}
