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
#' iaw$nearest(c(1, 5, 10), 7)   # returns 5
#'
#' # Exact match returns the value itself
#' iaw$nearest(c(1, 5, 10), 5)   # returns 5
#'
#' # Use with a grid of dates or thresholds
#' grid <- c(0, 0.25, 0.5, 0.75, 1.0)
#' iaw$nearest(grid, 0.37)        # returns 0.25
#'
#' # Equidistant: returns the first candidate (lower)
#' iaw$nearest(c(1, 3, 5), 2)     # returns 1
#'
#' # Snap a trade price to the nearest tick
#' tick_grid <- seq(100, 105, by = 0.25)
#' iaw$nearest(tick_grid, 102.33)  # 102.25
#'
#' # Find the closest standard maturity for a bond
#' maturities <- c(0.25, 0.5, 1, 2, 3, 5, 7, 10, 20, 30)
#' iaw$nearest(maturities, 4.2)   # 5
#'
#' # Works with negative values
#' iaw$nearest(c(-10, -5, 0, 5, 10), -3)  # -5

iaw$nearest <- function(nvec, target) {
    stopifnot(is.numeric(nvec), is.vector(nvec))
    stopifnot(is.numeric(target), length(target) == 1L)
    
    nvec[which.min(abs(nvec - target))]
}
