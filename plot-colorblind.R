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
#' iaw$colorblind(5)

iaw$colorblind <- function(n = 8) {
    stopifnot(is.numeric(n), length(n) == 1L, n > 0)
    palette <- c("black", "orange", "skyblue", "green", 
                 "yellow", "blue", "red", "gray")
    rep(palette, length.out = n)
}
