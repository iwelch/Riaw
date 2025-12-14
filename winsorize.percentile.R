#' Winsorize a Vector at Percentile Thresholds
#'
#' Clips (winsorizes) a numeric vector based on percentile thresholds. Values
#' below the minimum percentile are set to the value at that percentile; values
#' above the maximum percentile are set to the value at that percentile.
#'
#' @param x A numeric vector to winsorize.
#' @param xperc.min The lower percentile threshold (0-1). Default is 0.01 (1\%).
#'   Can be a vector of length 2 specifying \code{c(min, max)} percentiles.
#' @param xperc.max The upper percentile threshold (0-1). Default is 0.99 (99\%).
#'   Ignored if \code{xperc.min} is length 2.
#' @param verbose Logical; if TRUE, prints the computed winsorization levels.
#' @param name Optional character string naming the variable (for verbose output).
#'
#' @return A numeric vector of the same length as \code{x}, with extreme values
#'   clipped to the computed percentile thresholds.
#'
#' @details
#' This function first computes the values at the specified percentiles, then
#' calls \code{iaw$winsorize.level()} to perform the actual clipping.
#'
#' NA values in \code{x} are excluded when computing percentiles but preserved

#' in the output.
#'
#' @export
#'
#' @seealso \code{\link{iaw$winsorize.level}} for level-based winsorization,
#'   \code{\link{quantile}}
#'
#' @examples
#' # Winsorize at 1st and 99th percentiles (default)
#' set.seed(42)
#' x <- c(rnorm(98), -100, 100)  # Add outliers
#' x_win <- iaw$winsorize.percentile(x)
#' range(x)      # Shows outliers
#' range(x_win)  # Outliers clipped
#'
#' # Winsorize at 5th and 95th percentiles
#' x_win <- iaw$winsorize.percentile(x, 0.05, 0.95)
#'
#' # Using vector notation
#' x_win <- iaw$winsorize.percentile(x, c(0.05, 0.95))
#'
#' # With verbose output
#' x_win <- iaw$winsorize.percentile(x, 0.01, 0.99, verbose = TRUE)
#'
#' # Common financial application: winsorize returns
#' returns <- rnorm(1000, mean = 0.001, sd = 0.02)
#' returns_clean <- iaw$winsorize.percentile(returns, 0.01, 0.99)

iaw$winsorize.percentile <- function(x, xperc.min = 0.01, xperc.max = 0.99,
                                      verbose = FALSE, name = "") {
    # Handle 2-element vector input
    if (length(xperc.min) == 2) {
        xperc.max <- xperc.min[2]
        xperc.min <- xperc.min[1]
    }

    (is.numeric(x)) %or% "x must be numeric"
    (is.vector(x)) %or% "x must be a vector, not {{class(x)}}"
    (xperc.min < xperc.max) %or% "min percentile {{xperc.min}} must be < max {{xperc.max}}"
    (xperc.min < 0.75) %or% "min percentile {{xperc.min}} is too high"
    (xperc.max > 0.25) %or% "max percentile {{xperc.max}} is too low"

    x.nona <- x[!is.na(x)]
    lowest.set <- x.nona[rank(x.nona) < xperc.min * length(x.nona)]
    highest.set <- x.nona[rank(x.nona) > xperc.max * length(x.nona)]

    winsor.level.min <- if (length(lowest.set) == 0) -Inf else max(lowest.set)
    winsor.level.max <- if (length(highest.set) == 0) +Inf else min(highest.set)

    if (nchar(name) > 0) name <- paste("%", name)
    if (verbose) {
        cat("Winsorized", name, ": [",
            xperc.min, "=", winsor.level.min, ",",
            xperc.max, "=", winsor.level.max, "]\n")
    }

    iaw$winsorize.level(x, winsor.level.min, winsor.level.max, name = name)
}
