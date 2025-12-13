#' INRANGE
#'
#'  A perl-like 'and' condition that works with estrings as errors
#'
#' @name %inrange%
#'
#' @usage variable %inrange% c(2,10)
#'
#' @param the permitted range
#'
#' @return true or false
#'
#' @seealso  %or%, %and%
#'
#' @examples
#' 	x %inrange% c(-10,10)

'%inrange%' <- function(x, range_vector) ( (x >= range_vector[1]) & x <= (range_vector[2]) )
