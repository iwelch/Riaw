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

iaw$colSds <- function(x, na.rm = TRUE) {
    apply(x, 2, sd, na.rm = na.rm)
}
