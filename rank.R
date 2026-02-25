#' Rank with NA Handling
#'
#' @name rank
#'
#' Ranks values handling NAs.
#'
#' @param x Numeric vector.
#' @param ties.method Method for ties (default \code{"average"}).
#' @param ngroups If \code{NULL} (default), return ranks. If 0 or \code{"quantiles"},
#'   return quantile ranks (0--1). If a positive integer, return group numbers 1..ngroups.
#'
#' @return Vector of ranks, quantile ranks, or group numbers.
#'
#' @family statistics
#' @export
#'
#' @examples
#' # Basic rank with average ties (same as base rank default)
#' iaw$rank(c(3, 1, 4, 1, 5))
#'
#' # NA values are preserved (na.last = "keep")
#' iaw$rank(c(3, NA, 1, 4))
#'
#' # Ties method "first" breaks ties by position
#' iaw$rank(c(3, 1, 4, 1, 5), ties.method = "first")
#'
#' # Descending rank: negate the input
#' iaw$rank(-c(3, 1, 4, 1, 5))
#'
#' # Quantile ranks (0 to 1)
#' iaw$rank(c(10, 20, 30, 40, 50), ngroups = "quantiles")
#'
#' # Assign to 3 groups (portfolio formation)
#' iaw$rank(c(10, 20, 30, 40, 50, 60), ngroups = 3)

iaw$rank <- function(x, ties.method = "average", ngroups = NULL) {
    stopifnot(is.numeric(x))
    if (is.null(ngroups)) return(rank(x, na.last = "keep", ties.method = ties.method))
    quantiles <- rank(x, na.last = "keep") / sum(!is.na(x))
    if (ngroups == 0 || identical(ngroups, "quantiles")) return(quantiles)
    1L + as.integer(quantiles * (ngroups - 1e-6))
}
