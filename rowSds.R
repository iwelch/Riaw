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
#' iaw$rowSds(matrix(rnorm(20), nrow = 4))

iaw$rowSds <- function(x, na.rm = TRUE) {
    result <- apply(x, 1, sd, na.rm = na.rm)
    if (!is.null(rownames(x))) names(result) <- rownames(x)
    result
}
