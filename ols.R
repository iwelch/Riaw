#' Ordinary Least Squares Regression
#'
#' A fast interface for computing OLS regression coefficients. Supports both
#' formula and matrix/vector notation. This is a wrapper around
#' \code{iaw$ols.nona()} that handles missing values.
#'
#' @param y.or.f Either a formula (e.g., \code{y ~ x + z}) or a numeric vector
#'   of dependent variable values.
#' @param data If \code{y.or.f} is a formula, a data frame containing the
#'   variables. If \code{y.or.f} is a vector, a matrix or data frame of
#'   independent variables (one column per variable).
#' @param detail Integer controlling output verbosity:
#'   \itemize{
#'     \item 0: coefficients only (default)
#'     \item 1: coefficients and standard errors
#'     \item 2: full output including R-squared
#'   }
#'
#' @return Depends on \code{detail} parameter. See \code{\link{iaw$ols.nona}}
#'   for full details.
#'
#' @details
#' This function automatically handles missing values by removing observations
#' with any NA in the dependent or independent variables. For formula input,
#' an intercept is automatically included unless suppressed with \code{-1} or
#' \code{+0} in the formula.
#'
#' For large datasets or repeated regressions, \code{iaw$ols.nona()} or
#' \code{iaw$ols.fast()} may be more efficient.
#'
#' @export
#'
#' @seealso \code{\link{iaw$ols.nona}}, \code{\link{iaw$ols.fast}},
#'   \code{\link{iaw$olm}}, \code{\link{lm}}
#'
#' @examples
#' # Formula interface
#' d <- data.frame(y = rnorm(100), x = rnorm(100), z = rnorm(100))
#' iaw$ols(y ~ x + z, data = d)
#'
#' # More detail
#' iaw$ols(y ~ x + z, data = d, detail = 2)
#'
#' # Matrix/vector interface
#' y <- rnorm(100)
#' X <- cbind(1, rnorm(100), rnorm(100))  # Include intercept
#' iaw$ols(y, X)
#'
#' # No intercept
#' iaw$ols(y ~ x + z - 1, data = d)
#'
#' # Compare with lm
#' coef(lm(y ~ x + z, data = d))
#' iaw$ols(y ~ x + z, data = d)

iaw$ols <- function(y.or.f, data, detail = 0) {
    if (inherits(y.or.f, "formula")) {
        yx <- model.frame(y.or.f, data = data)
        y.or.f <- yx[, 1]
        data <- yx[, -1]
        if (attr(attr(yx, "terms"), "intercept")) {
            data <- cbind("(Intercept)" = 1, data)
        }
        rm(yx)
    }

    # Remove rows with any NA
    one.bad <- is.na(y.or.f) | apply(data, 1, function(x) any(is.na(x)))

    iaw$ols.nona(y.or.f[!one.bad], as.matrix(data[!one.bad, ]), detail)
}
