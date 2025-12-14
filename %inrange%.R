#' Check if Values are Within a Range
#'
#' @name %inrange%
#'
#' Tests whether values fall within a specified inclusive range.
#'
#' @param x A numeric value or vector to test.
#' @param range_vector A numeric vector of length 2: c(min, max).
#'
#' @return A logical vector.
#'
#' @family operators
#' @export
#'
#' @examples
#' 5 %inrange% c(1, 10)
#' c(-5, 0, 5, 10, 15) %inrange% c(0, 10)

`%inrange%` <- function(x, range_vector) {
    stopifnot(is.numeric(x))
    stopifnot(is.numeric(range_vector), length(range_vector) == 2L)
#    stopifnot( typeof(x) == typeof(range_vector) )
    (x >= range_vector[1]) & (x <= range_vector[2])
}
