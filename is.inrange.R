#' Test if Values Fall Within a Range
#'
#' @name is.inrange
#'
#' Returns logical vector for values strictly within range.
#'
#' @param x Numeric vector to test.
#' @param r Numeric vector of length 2: c(lower, upper).
#'
#' @return Logical vector.
#'
#' @family type-checking
#' @export
#'
#' @examples
#' iaw$is.inrange(1:10, c(3, 7))

iaw$is.inrange <- function(x, r) {
    stopifnot(is.numeric(x))
    stopifnot(is.numeric(r), length(r) == 2L)
    (r[1] <= r[2]) %or% "lower range must be <= upper range"
    (x > r[1]) & (x < r[2])
}
