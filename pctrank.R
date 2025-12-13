
#' Percentage Ranks
#'
#' @name rank for an alternative implementation
#'
#' Calculates percent ranks from 0 to 100 (for 100 percentiles, 101 categories!)
#'
#' @usage pctrank
#'
#' @param x a vector
#' @param numcategories usually 100.
#'
#' @return
#'
#' @examples table( pctrank( rnorm(1000000), 100 ) )
#'
#' @seealso iaw$rank for an alternative implementation

iaw$pctrank <- function(x, numcategories=100)  (as.integer((numcategories+1)*trunc(rank(x))/(length(x)+1) ))
