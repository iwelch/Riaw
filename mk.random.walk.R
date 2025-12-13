
#' Make a Random Walk
#'
#' @name mk.random.walk
#'
#' an error with reference to the correct function to use
#'
#' @seealso cumsum
#'

iaw$mk.random.walk <- function (N, drift = 0, sd = 1)
    iaw$abort("use cumsum(  rnorm(n=N, mean=drift, sd=sd)  )")

