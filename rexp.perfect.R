
#' Perfect Exponential Random Numbers
#'
#' @name rexp.perfect
#'
#' @param N number to draw
#' @param lambda probability (rate parameter)
#'
#' @return a vector of length N, containing only positive numbers
#'
#' @seealso rnorm.perfect, runif.perfect
#'

iaw$rexp.perfect <- function(N, lambda =1) {
    return( (sample(( qexp( seq(0,1,length.out=(N+2)), rate=lambda ) )[2:(N+1)])) )
}
