#' Check if Values are Within a Range
#'
#' Tests whether values fall within a specified inclusive range. Works with
#' vectors, returning a logical vector of the same length.
#'
#' @param x A numeric value or vector to test.
#' @param range_vector A numeric vector of length 2 specifying \code{c(min, max)}.
#'
#' @return A logical vector of the same length as \code{x}, with TRUE where
#'   values are within the range (inclusive) and FALSE otherwise.
#'
#' @export
#'
#' @seealso \code{\link{\%or\%}}, \code{\link{\%and\%}},
#'   \code{\link{iaw$is.inrange}}
#'
#' @examples
#' # Single value
#' 5 %inrange% c(1, 10)
#' # TRUE
#'
#' 15 %inrange% c(1, 10)
#' # FALSE
#'
#' # Vector of values
#' c(-5, 0, 5, 10, 15) %inrange% c(0, 10)
#' # FALSE TRUE TRUE TRUE FALSE
#'
#' # Use in subsetting
#' x <- 1:20
#' x[x %inrange% c(5, 15)]
#' # 5 6 7 8 9 10 11 12 13 14 15
#'
#' # Boundary values are included (inclusive range)
#' c(1, 10) %inrange% c(1, 10)
#' # TRUE TRUE

`%inrange%` <- function(x, range_vector) {
    (x >= range_vector[1]) & (x <= range_vector[2])
}
