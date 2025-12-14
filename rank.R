#' Rank with NA Handling
#'
#' @name rank
#'
#' Ranks values handling NAs.
#'
#' @param x Numeric vector.
#' @param ties.method Method for ties.
#'
#' @return Vector of ranks.
#'
#' @family statistics
#' @export
#'
#' @examples
#' iaw$rank(c(3, 1, 4, 1, 5))

iaw$rank <- function(x, ties.method = "average") {
    stopifnot(is.numeric(x))
    rank(x, na.last = "keep", ties.method = ties.method)
}
