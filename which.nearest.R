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
#' iaw$which.nearest(c(1, 5, 10), 7)   # 2 (value 5 is nearest to 7)
#'
#' # Use the index to retrieve the nearest element
#' v <- c(0.1, 0.5, 0.9, 1.5)
#' idx <- iaw$which.nearest(v, 0.8)
#' v[idx]                               # 0.9
#'
#' # Lookup nearest row in a sorted lookup table
#' knots <- c(0, 25, 50, 75, 100)
#' iaw$which.nearest(knots, 37)         # 2 (value 25)
#'
#' # Equidistant: first index wins (consistent with which.min)
#' iaw$which.nearest(c(1, 3, 5), 2)    # 1
#'
#' # Snap trade timestamps to nearest minute boundary
#' minute_grid <- seq(0, 3600, by = 60)   # seconds 0..3600
#' trade_sec <- 137
#' iaw$which.nearest(minute_grid, trade_sec)   # 3 (= 120 sec boundary)
#'
#' # Find nearest maturity from a yield curve
#' maturities <- c(0.25, 0.5, 1, 2, 5, 10, 30)
#' target <- 7
#' idx <- iaw$which.nearest(maturities, target)
#' maturities[idx]   # 5
#'
#' # Exact match returns that index
#' iaw$which.nearest(c(10, 20, 30), 20)   # 2

iaw$which.nearest <- function(nvec, your.number) {
    stopifnot(is.numeric(nvec), is.vector(nvec))
    stopifnot(is.numeric(your.number), length(your.number) == 1L)
    
    which.min(abs(nvec - your.number))
}
