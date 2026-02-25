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
#' # Default: clip at 1st and 99th percentile; the two outliers are pulled in
#' set.seed(1)
#' x <- c(rnorm(98), -100, 100)
#' range(x)
#' range(iaw$winsorize.percentile(x))
#'
#' # Tighter winsorization at 5th/95th percentile
#' iaw$winsorize.percentile(x, xperc = c(0.05, 0.95))
#'
#' # verbose = TRUE reports the actual cutoff values used
#' iaw$winsorize.percentile(x, verbose = TRUE, name = "returns")
#'
#' # NA values pass through unchanged
#' iaw$winsorize.percentile(c(NA, 1:10, NA))


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
