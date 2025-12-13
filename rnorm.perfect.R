
#' RNORM.PERFECT
#'
#' @name rnorm.perfect
#'
#'    perfectly spaced Gaussian normal random numbers
#'
#' @details
#' 	"rnorm.perfect" gives a normal draw with perfect distributional properties (mean, sd).  Otherwise, it is a substitute for rnorm.
#' 	This has limits.  For example, you cannot expect the distributional properties to vary across draws.
#' 	   sd( replicate( 1000, mean(rnorm(100)) ) )  -> 0.1
#' 	while
#' 	   sd( replicate( 1000, mean(rnorm.perfect(100)) ) )  -> 0.0
#'
#'    This function tries to be smarter than the average random draws: it skips the repeat function call to create appropriately spaced
#'    values, keeping only the reshuffle (sample), IF the number of requested observations does not change.
#'
#' @usage rnorm.perfect( n, mean= 0, sd= 1 )
#'
#' @param n: number of observations.  If (length(n)>1, the length is taken to be the number required
#' @param mean: scalar (not vector)
#' @param sd: scalar (not vector)
#'
#'  @return
#'
#'  @seealso rdraw.perfect runif.perfect r.perfect replicate
#'


make_rnorm_perfect <- function() {

  # internal persistent variable (acts like a static variable)
  baserands <- NULL

  rnorm_perfect <- function(N, mean = 0, sd = 1, speed.over.space = FALSE) {

    if (!speed.over.space) {
      return(sd * sample((qnorm(seq(0, 1, length.out = N + 2)))[2:(N + 1)]) + mean)
    }

    # initialize or regenerate baserands if NULL or wrong length
    if (is.null(baserands) || length(baserands) != N) {
      baserands <<- (qnorm(seq(0, 1, length.out = N + 2)))[2:(N + 1)]
    }

    # reshuffle for each call
    baserands <<- sample(baserands)

    sd * baserands + mean
  }

  return(rnorm_perfect)
}


# ---- Example usage ----

iaw$rnorm.perfect <- make_rnorm_perfect()
