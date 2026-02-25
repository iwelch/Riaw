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
#' @examples
#' # Rank a plain numeric vector (no groups)
#' iaw$dependent.rank(c(3, 1, 4, 1, 5))   # 3 1.5 4 1.5 5
#'
#' # Rank within groups -- each group gets its own 1..n ranks
#' x      <- c(10, 30, 20, 40, 15, 25)
#' groups <- c( "A", "A", "A", "B", "B", "B")
#' iaw$dependent.rank(x, groups)           # ranks within A and within B separately
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
