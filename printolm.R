#' Print OLS Regression Results
#'
#' A compact printer for regression results from \code{iaw$olm()}. Displays
#' coefficient table with optional customization of variable names and columns.
#'
#' @param ... Either a formula and data for \code{iaw$olm()}, or an existing
#'   \code{summary.lm} object from \code{iaw$olm()}.
#' @param yname Optional character string to label the dependent variable.
#' @param xnames Optional character vector of names for independent variables.
#' @param wantedcols Integer vector specifying which coefficient table columns
#'   to display. Default is 1:7 (all columns).
#' @param description Optional description string to print as header.
#'
#' @return Invisibly returns the adjusted R-squared value.
#'
#' @details
#' Prints a formatted output including:
#' \itemize{
#'   \item Coefficient table (estimates, std errors, t-stats, p-values, etc.)
#'   \item Adjusted R-squared
#'   \item Degrees of freedom and sample size
#'   \item Residual standard error
#' }
#'
#' @export
#'
#' @seealso \code{\link{iaw$olm}}, \code{\link{summary.lm}}
#'
#' @examples
#' # Basic usage
#' x <- rnorm(100)
#' y <- 2 + 3*x + rnorm(100)
#' iaw$printolm(y ~ x)
#'
#' # With custom names
#' iaw$printolm(y ~ x, yname = "Returns", xnames = "Market Beta")
#'
#' # With description
#' iaw$printolm(y ~ x, description = "CAPM Model")
#'
#' # From existing olm object
#' model <- iaw$olm(y ~ x)
#' iaw$printolm(model)

iaw$printolm <- function(..., yname = NULL, xnames = NULL, wantedcols = 1:7, description = NULL) {

    va <- list(...)
    if ((length(va) == 1) && inherits(va[[1]], "summary.lm")) {
        olmobject <- va[[1]]
    } else {
        olmobject <- iaw$olm(...)
    }

    ccc <- coef(olmobject)[, wantedcols, drop = FALSE]
    csigma <- olmobject$sigma

    if (!is.null(xnames)) {
        if (length(xnames) == nrow(ccc)) {
            rownames(ccc) <- xnames
        } else if (length(xnames) == nrow(ccc) - 1) {
            rownames(ccc) <- c("(Intercept)", xnames)
        } else {
            stop("Number of xnames (", length(xnames), ") doesn't match variables (", nrow(ccc), ")")
        }
    }

    if (!is.null(description)) cat("---------------- Model:", description, "\n")
    if (is.null(yname)) yname <- as.character(formula(olmobject))[2]
    cat("--- Explaining '", yname, "':\n", sep = "")
    print(ccc)
    cat("--- adj.R2=", olmobject$adj.r.squared,
        " df=", olmobject$df[2],
        " (N=", olmobject$df[2] + olmobject$df[1], ")",
        " sigma=", csigma,
        " RSS%=", olmobject$residssq, "\n\n", sep = "")

    invisible(olmobject$adj.r.squared)
}
