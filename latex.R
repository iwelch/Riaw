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
#'
#' # Regression summary table as LaTeX
#' fit <- lm(mpg ~ wt + hp, data = mtcars)
#' iaw$latex(summary(fit)$coefficients)
#'
#' # Correlation matrix for inclusion in a paper
#' cormat <- cor(mtcars[, c("mpg", "wt", "hp", "disp")])
#' iaw$latex(round(cormat, 2))
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
