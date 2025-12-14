#' Test if Values Fall Within a Range (Exclusive)
#'
#' Returns a logical vector indicating whether each element of x falls
#' strictly within the specified range (exclusive of boundaries).
#'
#' @param x A numeric vector to test.
#' @param r A numeric vector of length 2: \code{c(lower, upper)}.
#'
#' @return A logical vector the same length as \code{x}.
#'
#' @note Uses strict inequalities (< and >), so boundary values return FALSE.
#'   For inclusive boundaries, use \code{\link{\%inrange\%}}.
#'
#' @export
#'
#' @seealso \code{\link{\%inrange\%}} for inclusive range testing
#'
#' @examples
#' x <- 1:10
#'
#' # Strict inequality (exclusive)
#' iaw$is.inrange(x, c(3, 7))
#' # FALSE FALSE FALSE TRUE TRUE TRUE FALSE FALSE FALSE FALSE
#' # Note: 3 and 7 are FALSE
#'
#' # Compare to %inrange% (inclusive)
#' x %inrange% c(3, 7)
#' # FALSE FALSE TRUE TRUE TRUE TRUE TRUE FALSE FALSE FALSE

iaw$is.inrange <- function(x, r) {
    (length(r) == 2) %or% "range must be vector of length 2"
    (r[1] <= r[2]) %or% "lower range must be <= upper range: {{r}}"
    (x > r[1]) & (x < r[2])
}
