#' Find Nearest Value
#'
#' @name nearest
#'
#' Returns the nearest value in a vector.
#'
#' @param nvec Numeric vector to search.
#' @param target Target value.
#'
#' @return Nearest value.
#'
#' @family utilities
#' @export
#'
#' @examples
#' iaw$nearest(c(1, 5, 10), 7)

iaw$nearest <- function(nvec, target) {
    stopifnot(is.numeric(nvec), is.vector(nvec))
    stopifnot(is.numeric(target), length(target) == 1L)
    
    nvec[which.min(abs(nvec - target))]
}
