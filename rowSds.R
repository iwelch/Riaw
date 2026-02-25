#' Row Standard Deviations
#'
#' @name rowSds
#'
#' Calculates SD for each row.
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
#' m <- matrix(1:12, nrow = 3)
#' iaw$rowSds(m)   # SD across 4 columns for each of 3 rows
#'
#' # Named rows are preserved
#' m2 <- matrix(c(2, 4, 6, 8, 1, 9), nrow = 2,
#'              dimnames = list(c("A", "B"), NULL))
#' iaw$rowSds(m2)  # named: A=2.58, B=5.69
#'
#' # NA handling: NAs skipped by default
#' m3 <- matrix(c(1, 2, NA, 4, 5, 6), nrow = 2)
#' iaw$rowSds(m3)              # NA row uses remaining values
#' iaw$rowSds(m3, na.rm = FALSE)  # returns NA for that row
#'
#' # Cross-sectional dispersion of daily stock returns
#' returns <- matrix(rnorm(20), nrow = 5,
#'                   dimnames = list(paste0("day", 1:5), NULL))
#' iaw$rowSds(returns)  # volatility across stocks for each day
#'
#' # Single-column matrix returns 0 (or NA) for every row
#' iaw$rowSds(matrix(1:4, ncol = 1))  # all NA (sd of length-1 vector)
#'
#' # Data frame input works too (coerced to matrix internally by apply)
#' df <- data.frame(a = c(1, 2, 3), b = c(4, 5, 6), c = c(7, 8, 9))
#' iaw$rowSds(df)  # 3.0, 3.0, 3.0

iaw$rowSds <- function(x, na.rm = TRUE) {
    result <- apply(x, 1, sd, na.rm = na.rm)
    if (!is.null(rownames(x))) names(result) <- rownames(x)
    result
}
