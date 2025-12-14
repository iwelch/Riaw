#' Find Nearest Value
#'
#' Finds the value in a vector that is closest to a target number.
#'
#' @param nvec A numeric vector to search.
#' @param your.number The target number to find the nearest match for.
#'
#' @return The value in nvec that is closest to your.number.
#'
#' @export
#'
#' @seealso \code{\link{iaw$which.nearest}}, \code{\link{which.min}}
#'
#' @examples
#' x <- c(1.2, 3.5, 7.8, 12.1, 15.6)
#'
#' iaw$nearest(x, 8)
#' # 7.8
#'
#' iaw$nearest(x, 10)
#' # 12.1

iaw$nearest <- function(nvec, your.number) {
    nvec[data.table::first(iaw$which.nearest(nvec, your.number))]
}

#' Find Index of Nearest Value
#'
#' Finds the index of the value in a vector that is closest to a target number.
#'
#' @param nvec A numeric vector to search.
#' @param your.number The target number to find the nearest match for.
#'
#' @return Integer index of the nearest element.
#'
#' @export
#'
#' @seealso \code{\link{iaw$nearest}}, \code{\link{which.min}}
#'
#' @examples
#' x <- c(1.2, 3.5, 7.8, 12.1, 15.6)
#'
#' iaw$which.nearest(x, 8)
#' # 3
#'
#' x[iaw$which.nearest(x, 10)]
#' # 12.1

iaw$which.nearest <- function(nvec, your.number) {
    (is.numeric(nvec) & is.vector(nvec)) %or%
        "nvec must be a numeric vector, not {{iaw$whatis(nvec)}}"
    which.min(abs(nvec - your.number))
}
