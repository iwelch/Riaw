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
#' x <- c(-100, 1, 2, 3, 100)
#' iaw$winsorize.level(x, c(0, 10))

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
