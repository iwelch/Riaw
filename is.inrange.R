
#' IS.INRANGE
#'
#' @name is.inrange
#'
#'  returns boolean vector for whether vector x values lie between a range
#'
#' @usage is.inrange( x, range )
#'
#' @param x the numeric vector
#' @param r the range
#'
#' @return vector of Boolean
#'


iaw$is.inrange <- function(x, r) {
    (length(r)==2) %or% "range must be vector of length 2"
    (r[1] <= r[2]) %or% "lower range must be below upper range on {{r}}"
    (x>r[1]) & (x<r[2])
}

