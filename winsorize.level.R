#' Winsorize at Fixed Levels
#'
#' @name winsorize.level
#'
#' Clips values to specified min/max.
#'
#' @param x Numeric vector.
#' @param xmin Minimum value or c(min, max).
#' @param xmax Maximum value (ignored if xmin is length 2).
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
#' iaw$winsorize.level(x, 0, 10)

iaw$winsorize.level <- function(x, xmin, xmax = NULL, name = NULL, verbose = FALSE) {
    stopifnot(is.numeric(x))
    
    if (length(xmin) == 2) {
        xmax <- xmin[2]
        xmin <- xmin[1]
    }
    
    stopifnot(!is.null(xmax))
    stopifnot(all(xmin <= xmax))
    
    if (verbose) {
        if (is.null(name)) name <- "x"
        cat("[winsorize.level]", name, ":",
            length(x), "values,", sum(!is.na(x)), "non-NA\n")
        cat("  Bottom[<", xmin, "]: N=", sum(x < xmin, na.rm = TRUE),
            " Top[>", xmax, "]: N=", sum(x > xmax, na.rm = TRUE), "\n")
    }
    
    pmin(pmax(x, xmin), xmax)
}
