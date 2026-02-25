#' Column Standard Deviations
#'
#' @name colSds
#'
#' Calculates SD for each column.
#'
#' @param x Matrix or data frame.
#' @param na.rm Remove NA values.
#'
#' @return Vector of standard deviations.
#'
#' @family statistics
#' @export
#'
#' @examples
#' # 5x4 matrix: one SD per column
#' set.seed(1)
#' m <- matrix(rnorm(20), nrow = 5, ncol = 4)
#' iaw$colSds(m)
#'
#' # Data frame of numeric columns
#' df <- data.frame(a = c(1, 2, 3, 4, 5), b = c(2, 2, 2, 2, 2))
#' iaw$colSds(df)   # b has SD = 0
#'
#' # NA values are ignored by default (na.rm = TRUE)
#' df2 <- data.frame(x = c(1, NA, 3), y = c(4, 5, 6))
#' iaw$colSds(df2)
#'
#' # Annualized volatility from daily returns (sqrt(252) scaling)
#' set.seed(42)
#' daily <- data.frame(stock_A = rnorm(252, 0, 0.01), stock_B = rnorm(252, 0, 0.02))
#' iaw$colSds(daily) * sqrt(252)  # annualized vol ~ 0.16, 0.32
#'
#' # Constant column has zero standard deviation
#' iaw$colSds(data.frame(x = rep(5, 10)))  # 0
#'
#' # Compare with na.rm = FALSE: returns NA if any missing
#' df3 <- data.frame(a = c(1, NA, 3))
#' iaw$colSds(df3, na.rm = FALSE)  # NA

# Better - use matrixStats (10-100x faster)
iaw$colSds <- function(x, na.rm = TRUE) {
    if (requireNamespace("matrixStats", quietly = TRUE)) {
        matrixStats::colSds(as.matrix(x), na.rm = na.rm)
    } else {
        apply(x, 2, sd, na.rm = na.rm)
    }
}
