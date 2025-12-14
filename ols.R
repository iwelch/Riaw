#' Ordinary Least Squares Regression
#'
#' @name ols
#'
#' Fast OLS regression with formula or matrix interface.
#'
#' @param y.or.f Formula or numeric vector of dependent variable.
#' @param data Data frame (for formula) or matrix of regressors.
#' @param detail Output verbosity: 0=coefs, 1=+SE, 2=+Rsq.
#'
#' @return Depends on detail parameter.
#'
#' @family regression
#' @export
#'
#' @examples
#' d <- data.frame(y = rnorm(100), x = rnorm(100))
#' iaw$ols(y ~ x, data = d)

iaw$ols <- function(y.or.f, data, detail = 0) {
    stopifnot(is.numeric(detail), length(detail) == 1L, detail %in% 0:4)
    
    if (inherits(y.or.f, "formula")) {
        yx <- model.frame(y.or.f, data = data)
        y.or.f <- yx[, 1]
        data <- yx[, -1]
        if (attr(attr(yx, "terms"), "intercept")) {
            data <- cbind("(Intercept)" = 1, data)
        }
        rm(yx)
    }
    
    stopifnot(is.numeric(y.or.f))
    stopifnot(is.matrix(data) || is.data.frame(data))
    
    one.bad <- is.na(y.or.f) | apply(data, 1, function(x) any(is.na(x)))
    iaw$ols.nona(y.or.f[!one.bad], as.matrix(data[!one.bad, ]), detail)
}
