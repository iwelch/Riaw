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

iaw$rowSds <- function(x, na.rm = TRUE) {
    result <- apply(x, 1, sd, na.rm = na.rm)
    if (!is.null(rownames(x))) names(result) <- rownames(x)
    result
}
