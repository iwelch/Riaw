#' Enhanced OLS with Newey-West Standard Errors
#'
#' @name olm
#'
#' OLS regression with Newey-West HAC standard errors.
#'
#' @param ... Arguments passed to lm().
#' @param newey.west Lag for Newey-West SE. Default 0.
#' @param stdcoefs Include standardized coefficients.
#' @param include.anova Include ANOVA decomposition.
#' @param keep.pval Keep p-values.
#' @param truncate.T Truncate t-statistics.
#' @param digits Rounding digits.
#'
#' @return Modified summary.lm object.
#'
#' @family regression
#' @export
#'
#' @importFrom lmtest coeftest
#' @importFrom sandwich NeweyWest
#'
#' @examples
#' df <- data.frame(y = rnorm(100), x = rnorm(100))
#' iaw$olm(y ~ x, data = df)

iaw$olm <- function(..., newey.west = 0, stdcoefs = TRUE, include.anova = TRUE,
                    keep.pval = FALSE, truncate.T = TRUE, digits = 4) {
    stopifnot(is.numeric(newey.west), length(newey.west) == 1L)
    stopifnot(is.logical(stdcoefs), length(stdcoefs) == 1L)
    stopifnot(is.logical(include.anova), length(include.anova) == 1L)
    stopifnot(is.logical(keep.pval), length(keep.pval) == 1L)
    stopifnot(is.logical(truncate.T), length(truncate.T) == 1L)
    stopifnot(is.numeric(digits), length(digits) == 1L)
    
    if (!requireNamespace("lmtest", quietly = TRUE)) {
        stop("Package 'lmtest' required")
    }
    if (!requireNamespace("sandwich", quietly = TRUE)) {
        stop("Package 'sandwich' required")
    }
    
    lmo <- stats::lm(..., x = TRUE, na.action = na.exclude)
    
    if (newey.west >= 0) {
        nw.se <- lmtest::coeftest(lmo, 
            vcov = sandwich::NeweyWest(lmo, lag = newey.west, prewhite = FALSE))[, 2]
    }
    
    if (stdcoefs) {
        lmo.sx <- apply(lmo$x[, -1, drop = FALSE], 2, sd, na.rm = TRUE)
        lmo.sy <- sd(lmo$model[[1]], na.rm = TRUE)
        scoef <- coef(lmo)[-1] * lmo.sx / lmo.sy
    }
    
    slmo <- summary(lmo)
    coeftable <- coef(slmo)
    colnames(coeftable) <- c("coefest", "se.ols", "T.ols", "pval.ols")
    
    if (newey.west >= 0) {
        coeftable <- cbind(coeftable, "se.nw" = nw.se)
        coeftable <- cbind(coeftable, "T.nw" = coeftable[, 1] / nw.se)
    }
    
    if (stdcoefs) {
        coeftable <- cbind(coeftable, "stdcoefs" = c(NA, scoef))
    }
    
    if (!keep.pval) {
        coeftable <- coeftable[, !grepl("pval", colnames(coeftable)), drop = FALSE]
    }
    
    if (truncate.T) {
        tcols <- grep("^T\\.", colnames(coeftable))
        coeftable[, tcols] <- pmin(pmax(coeftable[, tcols], -99.99), 99.99)
    }
    
    slmo$coefficients <- round(coeftable, digits)
    
    if (include.anova) {
        a <- anova(lmo)
        slmo$residssq <- round(100 * a[nrow(a), 2] / sum(a[, 2]), 1)
    }
    
    slmo
}
