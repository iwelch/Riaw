#' Winsorize at Fixed Levels
#'
#' @name winsorize.level
#'
#' Clips values to specified min/max.
#'
#' @param x Numeric vector.
#' @param lvlminmax Numeric vector of length 2: c(min, max).
#' @param name Variable name for verbose output.
#' @param verbose Print summary.
#'
#' @return Numeric vector.
#'
#' @family data-transformation
#' @export
#'
#' @examples
#' # Basic clipping: extremes pulled to bounds
#' x <- c(-100, 1, 2, 3, 100)
#' iaw$winsorize.level(x, c(0, 10))
#'
#' # Typical finance use: clip returns to +/-50%
#' returns <- c(-0.8, -0.2, 0.0, 0.15, 1.2)
#' iaw$winsorize.level(returns, c(-0.5, 0.5))
#'
#' # NA values are preserved
#' iaw$winsorize.level(c(NA, -5, 0, 5, NA), c(-2, 2))
#'
#' # verbose = TRUE shows how many values were clipped
#' iaw$winsorize.level(c(-10, 1, 2, 3, 10), c(0, 5),
#'                     name = "myvar", verbose = TRUE)

iaw$winsorize.level <- function(x, lvlminmax, name = NULL, verbose = FALSE) {
    stopifnot(is.numeric(x), length(lvlminmax)==2L)
    stopifnot(lvlminmax[1] < lvlminmax[2])  ## equal would be crazy

    if (verbose) {
        if (is.null(name)) name <- "x"
        cat("[winsorize.level]", name, ":", length(x), "values,", sum(!is.na(x)), "non-NA\n")
        cat("  Bottom[<", lvlminmax[1], "]: N=", sum(x < lvlminmax[1], na.rm = TRUE),
            " Top[>", lvlminmax[2], "]: N=", sum(x > lvlminmax[2], na.rm = TRUE), "\n")
    }

    pmin(pmax(x, lvlminmax[1]), lvlminmax[2])
}
