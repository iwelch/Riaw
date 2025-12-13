
#' DEPENDENT.RANK
#'
#' @name dependent.rank
#'
#' ranks of numeric variables, categorized by some factor
#'
#' @usage dependent.rank(INDEX, x)
#'
#' @param INDEX a factor
#' @param x a numeric
#'
#' @return an equal length vector
#'
#' @seealso ave
#'

iaw$dependent.rank <- function (INDEX, x)
{
  (is.factor(INDEX)) %or% "dependent.rank: not is factor INDEX {{class(INDEX)}}"
  (is.numeric(x)) %or% "dependent.rank: not is factor x {{class(x)}}"
  ds <- data.frame(INDEX, x)
  return( ave(ds[[2]], ds[[1]], FUN = rank) )
}
