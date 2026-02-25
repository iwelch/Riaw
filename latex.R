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
#' @examples
#' \dontrun{
#' # Print a data frame as a LaTeX table (requires xtable)
#' df <- data.frame(x = 1:3, y = c(0.1, 0.2, 0.3))
#' iaw$latex(df)
#'
#' # Print a matrix as a LaTeX table
#' m <- matrix(1:6, nrow = 2, dimnames = list(c("r1","r2"), c("a","b","c")))
#' iaw$latex(m)
#' }
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
