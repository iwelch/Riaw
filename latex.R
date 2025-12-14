#' LaTeX Output Utilities
#'
#' @name latex
#'
#' Utilities for LaTeX output.
#'
#' @param x Object to format.
#'
#' @return LaTeX string.
#'
#' @family utilities
#' @export

iaw$latex <- function(x) {
    if (requireNamespace("xtable", quietly = TRUE)) {
        print(xtable::xtable(x), type = "latex")
    } else {
        warning("Package 'xtable' required")
    }
}
