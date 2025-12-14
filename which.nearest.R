#' Find Index of Nearest Value
#'
#' @name which.nearest
#'
#' Returns index of nearest value in vector.
#'
#' @param nvec Numeric vector to search.
#' @param your.number Target value.
#'
#' @return Integer index.
#'
#' @family utilities
#' @export
#'
#' @examples
#' iaw$which.nearest(c(1, 5, 10), 7)

iaw$which.nearest <- function(nvec, your.number) {
    stopifnot(is.numeric(nvec), is.vector(nvec))
    stopifnot(is.numeric(your.number), length(your.number) == 1L)
    
    which.min(abs(nvec - your.number))
}
