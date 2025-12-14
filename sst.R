#' Total Sum of Squares
#'
#' @name sst
#'
#' Calculates total sum of squares.
#'
#' @param x Numeric vector.
#'
#' @return SST value.
#'
#' @family statistics
#' @export
#'
#' @examples
#' iaw$sst(c(1, 2, 3, 4, 5))

iaw$sst <- function(x) {
    stopifnot(is.numeric(x))
    sum((x - mean(x, na.rm = TRUE))^2, na.rm = TRUE)
}
