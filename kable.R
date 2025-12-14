#' Kable Wrapper
#'
#' @name kable
#'
#' Wrapper for knitr::kable.
#'
#' @param x Table to format.
#' @param ... Kable arguments.
#'
#' @return Formatted table.
#'
#' @family utilities
#' @export

iaw$kable <- function(x, ...) {
    if (requireNamespace("knitr", quietly = TRUE)) {
        knitr::kable(x, ...)
    } else {
        print(x)
    }
}
