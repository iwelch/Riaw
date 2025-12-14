#' Winsorize a Vector at Fixed Levels
#'
#' Clips (winsorizes) a numeric vector to specified minimum and maximum values.
#' Values below the minimum are set to the minimum; values above the maximum
#' are set to the maximum. NA values are preserved.
#'
#' @param x A numeric vector to winsorize.
#' @param xmin The minimum threshold. Can be a single value or a vector of
#'   length 2 specifying \code{c(min, max)}.
#' @param xmax The maximum threshold. Ignored if \code{xmin} is length 2.
#' @param name Optional character string naming the variable (for verbose output).
#' @param verbose Logical; if TRUE, prints summary of winsorization.
#'
#' @return A numeric vector of the same length as \code{x}, with extreme values
#'   clipped to the specified range.
#'
#' @export
#'
#' @seealso \code{\link{iaw$winsorize.percentile}} for percentile-based winsorization,
#'   \code{\link{pmin}}, \code{\link{pmax}}
#'
#' @examples
#' # Basic winsorization
#' x <- c(-100, 1, 2, 3, 4, 5, 1000)
#' iaw$winsorize.level(x, 0, 10)
#' # 0 1 2 3 4 5 10
#'
#' # Using vector notation for bounds
#' iaw$winsorize.level(x, c(0, 10))
#' # 0 1 2 3 4 5 10
#'
#' # NA values are preserved
#' x <- c(1, NA, 100, 2)
#' iaw$winsorize.level(x, 0, 50)
#' # 1 NA 50 2
#'
#' # Verbose output shows counts
#' x <- rnorm(1000)
#' iaw$winsorize.level(x, -2, 2, name = "x", verbose = TRUE)

iaw$winsorize.level <- function(x, xmin, xmax = NULL, name = NULL, verbose = FALSE) {
    # Handle 2-element vector input
    if (length(xmin) == 2) {
        xmax <- xmin[2]
        xmin <- xmin[1]
    }

    (!is.null(xmax)) %or% "winsorize.level needs a max argument"
    (all(xmin <= xmax)) %or% "winsorize.level range error: min {{head(xmin)}} > max {{head(xmax)}}"
    (is.vector(x)) %or% "winsorize.level: x must be a vector, not {{class(x)}}"

    if (verbose) {
        if (is.null(name)) name <- "unknown"
        cat("[winsorize.level]", name, ":",
            length(x), "values,", sum(!is.na(x)), "non-NA: ")
        tb <- sum(x < xmin, na.rm = TRUE)
        tt <- sum(x > xmax, na.rm = TRUE)
        cat("Bottom[<", xmin, "]: N=", tb,
            " Top[>", xmax, "]: N=", tt, "\n")
    }

    pmin(pmax(x, xmin), xmax)
}
