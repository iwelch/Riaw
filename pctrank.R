#' Percentile Rank
#'
#' @name pctrank
#'
#' Computes percentile rank (0-1).
#'
#' @param x Numeric vector.
#'
#' @return Vector of percentile ranks.
#'
#' @family statistics
#' @export
#'
#' @examples
#' iaw$pctrank(c(10, 20, 30, 40, 50))

iaw$pctrank <- function(x) {
    .Defunct("use dplyr::percent_rank(x)")
    stopifnot(is.numeric(x))
    (rank(x, na.last = "keep") - 1) / (sum(!is.na(x)) - 1)
}
