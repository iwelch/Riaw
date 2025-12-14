#' Winsorize at Percentile Thresholds
#'
#' @name winsorize.percentile
#'
#' Clips values at specified percentiles.
#'
#' @param x Numeric vector.
#' @param xperc.min Lower percentile (0-1). Default 0.01.
#' @param xperc.max Upper percentile (0-1). Default 0.99.
#' @param verbose Print summary.
#' @param name Variable name for output.
#'
#' @return Numeric vector.
#'
#' @family data-transformation
#' @export
#'
#' @examples
#' x <- c(rnorm(98), -100, 100)
#' iaw$winsorize.percentile(x)

iaw$winsorize.percentile <- function(x, xperc.min = 0.01, xperc.max = 0.99,
                                      verbose = FALSE, name = "") {
    stopifnot(is.numeric(x))
    
    if (length(xperc.min) == 2) {
        xperc.max <- xperc.min[2]
        xperc.min <- xperc.min[1]
    }
    
    stopifnot(is.numeric(xperc.min), length(xperc.min) == 1L)
    stopifnot(is.numeric(xperc.max), length(xperc.max) == 1L)
    stopifnot(xperc.min < xperc.max)
    stopifnot(xperc.min < 0.75)
    stopifnot(xperc.max > 0.25)
    
    x.nona <- x[!is.na(x)]
    lowest.set <- x.nona[rank(x.nona) < xperc.min * length(x.nona)]
    highest.set <- x.nona[rank(x.nona) > xperc.max * length(x.nona)]
    
    winsor.level.min <- if (length(lowest.set) == 0) -Inf else max(lowest.set)
    winsor.level.max <- if (length(highest.set) == 0) +Inf else min(highest.set)
    
    if (verbose) {
        cat("Winsorized", name, ": [",
            xperc.min, "=", winsor.level.min, ",",
            xperc.max, "=", winsor.level.max, "]\n")
    }
    
    iaw$winsorize.level(x, winsor.level.min, winsor.level.max, name = name)
}
