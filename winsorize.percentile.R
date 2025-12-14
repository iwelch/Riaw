#' Winsorize at Percentile Thresholds
#'
#' @name winsorize.percentile
#'
#' Clips values at specified percentiles.
#'
#' @param x Numeric vector.
#' @param xperc vector of Lower percentile (0-1). Default 0.01, upper percentile
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


iaw$winsorize.percentile <- function(x, xperc = c(0.01, 0.99), verbose = FALSE, name = "") {
    stopifnot(is.numeric(x))
    stopifnot(is.numeric(xperc), length(xperc) == 2L)
    stopifnot(xperc[1] < xperc[2])
    stopifnot(xperc[1] < 0.75); stopifnot(xperc[2] > 0.25)

    x.nona <- x[!is.na(x)]

    # Use quantile directly - this is the standard approach
    winsor.level <- quantile(x.nona, probs = xperc, names = FALSE)

    if (verbose) {
        cat("Winsorized", name, ": [",
            xperc[1], "=", winsor.level[1], ",",
            xperc[2], "=", winsor.level[2], "]\n")
    }

    iaw$winsorize.level(x, winsor.level, verbose = FALSE, name = name)
}
