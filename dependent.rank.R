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
#' # Rank stock returns within each sector for cross-sectional analysis
#' returns <- c(0.05, 0.12, -0.02, 0.08, 0.01, -0.03)
#' sector  <- c("Tech", "Tech", "Tech", "Fin", "Fin", "Fin")
#' iaw$dependent.rank(returns, sector)  # ranks 1-3 within each sector
#'
#' # Ties receive average ranks (default R behavior)
#' iaw$dependent.rank(c(5, 5, 1, 3))  # 3.5 3.5 1.0 2.0
#'
#' # Single group is equivalent to no grouping
#' iaw$dependent.rank(c(30, 10, 20), groups = c("X", "X", "X"))  # 3 1 2
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
