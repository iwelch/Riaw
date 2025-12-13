
#' WHICH.NEAREST
#'
#' @name which.nearest
#'
#'   find the index of the nearest value in a vector.
#'
#' @usage which.nearest (haystack, needle)
#'
#' @param nvec the haystack vector
#' @param your.number the needle you want to find
#'
#' @return one or more integers that index the element(s) with the smallest distance from the needle
#'
#' @seealso is.instring, which.variable, nearest
#'

iaw$which.nearest <- function (nvec, your.number)  {
    (is.numeric(nvec)&is.vector(nvec)) %or% "which.nearest must be called with a numeric vector, not a {{whatis(nvec)}}"
    which.min( abs(nvec - your.number) )
}
