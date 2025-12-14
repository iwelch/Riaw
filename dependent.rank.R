#' Dependent Variable Rank
#'
#' @name dependent.rank
#'
#' Ranks within groups.
#'
#' @param x Values to rank.
#' @param groups Grouping variable.
#'
#' @return Vector of ranks.
#'
#' @family statistics
#' @export

iaw$dependent.rank <- function(x, groups = NULL) {
    stopifnot(is.numeric(x))
    if (is.null(groups)) {
        rank(x)
    } else {
        ave(x, groups, FUN = rank)
    }
}
