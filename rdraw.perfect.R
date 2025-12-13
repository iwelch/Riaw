
#' Perfect Random Uniforms
#'
#' @name rdraw.perfect
#'
#' random numbers from EITHER 0 OR 1 that are perfectly sampled according to the probability p, but appropriately shuffled
#'
#' @param N number to draw
#' @param prob probability
#'
#' @return a vector of length N containing only 0 or 1, with frequency = prob
#'
#' @seealso rnorm.perfect, runif.perfect
#'

iaw$rdraw.perfect <- function(N, p) {
    N1 <- as.integer( N*p ); N0 <- N-N1
    sample( c( rep(1, N1), rep(0, N0) ) )
}
