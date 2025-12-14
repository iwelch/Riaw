#' Rank Within Groups
#'
#' Calculates ranks of a numeric variable within groups defined by a factor.
#' Useful for within-group rankings in panel data.
#'
#' @param INDEX A factor defining groups.
#' @param x A numeric vector to rank.
#'
#' @return A numeric vector of the same length containing within-group ranks.
#'
#' @export
#'
#' @seealso \code{\link{ave}}, \code{\link{rank}}, \code{\link{iaw$pctrank}}
#'
#' @examples
#' # Rank students within each class
#' df <- data.frame(
#'     class = factor(c("A", "A", "A", "B", "B", "B")),
#'     score = c(85, 92, 78, 88, 95, 82)
#' )
#' df$rank_in_class <- iaw$dependent.rank(df$class, df$score)
#' df

iaw$dependent.rank <- function(INDEX, x) {
    (is.factor(INDEX)) %or% "INDEX must be a factor, not {{class(INDEX)}}"
    (is.numeric(x)) %or% "x must be numeric, not {{class(x)}}"
    ds <- data.frame(INDEX, x)
    ave(ds[[2]], ds[[1]], FUN = rank)
}
