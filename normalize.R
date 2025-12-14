#' Normalize/Standardize (Deprecated - Use scale())
#'
#' This function is deprecated. Use \code{scale()} instead for standardizing
#' variables to mean 0 and standard deviation 1.
#'
#' @param object Object to normalize.
#'
#' @return Throws an error directing you to use \code{scale()}.
#'
#' @export
#'
#' @seealso \code{\link{scale}}
#'
#' @examples
#' # Instead of normalize(), use:
#' x <- rnorm(100, mean = 50, sd = 10)
#' x_standardized <- scale(x)
#' mean(x_standardized)  # approximately 0
#' sd(x_standardized)    # approximately 1

iaw$normalize <- function(object) {
    iaw$abort("Use scale(object) instead of normalize(object)")
}
