#' Print OLS Regression Results
#'
#' @name printolm
#'
#' Compact printer for olm results.
#'
#' @param ... Formula/data or summary.lm object.
#' @param yname Label for dependent variable.
#' @param xnames Labels for independent variables.
#' @param wantedcols Columns to display (1:7).
#' @param description Model description.
#'
#' @return Invisibly returns adjusted R-squared.
#'
#' @family regression
#' @export
#'
#' @examples
#' df <- data.frame(y = rnorm(100), x = rnorm(100))
#' iaw$printolm(y ~ x, data = df)

iaw$printolm <- function(..., yname = NULL, xnames = NULL, 
                          wantedcols = 1:7, description = NULL) {
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
            stop("xnames length mismatch")
        }
    }
    
    if (!is.null(description)) cat("---- Model:", description, "\n")
    if (is.null(yname)) yname <- as.character(formula(olmobject))[2]
    cat("--- Explaining '", yname, "':\n", sep = "")
    print(ccc)
    cat("--- adj.R2=", olmobject$adj.r.squared,
        " df=", olmobject$df[2],
        " N=", olmobject$df[2] + olmobject$df[1],
        " sigma=", csigma, "\n\n", sep = "")
    
    invisible(olmobject$adj.r.squared)
}
