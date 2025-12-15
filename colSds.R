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
#' iaw$colSds(matrix(rnorm(20), ncol = 4))

# Better - use matrixStats (10-100x faster)
iaw$colSds <- function(x, na.rm = TRUE) {
    if (requireNamespace("matrixStats", quietly = TRUE)) {
        matrixStats::colSds(as.matrix(x), na.rm = na.rm)
    } else {
        apply(x, 2, sd, na.rm = na.rm)
    }
}
