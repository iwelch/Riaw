#' Enhanced OLS Regression with Newey-West Standard Errors
#'
#' Runs OLS regression with optional Newey-West heteroskedasticity and
#' autocorrelation consistent (HAC) standard errors, standardized coefficients,
#' and ANOVA decomposition.
#'
#' @param ... Arguments passed to \code{lm()}, typically a formula and data.
#' @param newey.west Integer lag for Newey-West standard errors. Default 0
#'   uses Newey-West with automatic lag. Set to negative to disable.
#' @param stdcoefs Logical; if TRUE (default), includes standardized coefficients.
#' @param include.anova Logical; if TRUE (default), includes percent sum of squares.
#' @param keep.pval Logical; if TRUE, keeps p-values in output. Default FALSE.
#' @param truncate.T Logical; if TRUE (default), truncates t-statistics to [-99.99, 99.99].
#' @param digits Integer; number of decimal places for rounding. Default 4.
#'
#' @return A modified \code{summary.lm} object with additional columns:
#'   \itemize{
#'     \item \code{coefest}: Coefficient estimates
#'     \item \code{se.ols}: OLS standard errors
#'     \item \code{T.ols}: OLS t-statistics
#'     \item \code{se.nw(k)}: Newey-West standard errors (if enabled)
#'     \item \code{T.nw(k)}: Newey-West t-statistics (if enabled)
#'     \item \code{stdcoefs}: Standardized coefficients (if enabled)
#'     \item \code{pctsumsq}: Percent of sum of squares (if ANOVA enabled)
#'   }
#'
#' @details
#' Newey-West standard errors are robust to heteroskedasticity and autocorrelation,
#' making them suitable for time series regressions.
#'
#' Standardized coefficients show the effect in standard deviation units,
#' allowing comparison across variables with different scales.
#'
#' @export
#'
#' @seealso \code{\link{iaw$printolm}}, \code{\link{lm}}, \code{\link[sandwich]{NeweyWest}}
#'
#' @examples
#' # Basic regression with Newey-West SEs
#' df <- data.frame(y = rnorm(100), x1 = rnorm(100), x2 = rnorm(100))
#' result <- iaw$olm(y ~ x1 + x2, data = df)
#' result$coefficients
#'
#' # Without Newey-West
#' result <- iaw$olm(y ~ x1 + x2, data = df, newey.west = -1)
#'
#' # With specific lag
#' result <- iaw$olm(y ~ x1 + x2, data = df, newey.west = 4)

iaw$olm <- function(..., newey.west = 0, stdcoefs = TRUE, include.anova = TRUE,
                     keep.pval = FALSE, truncate.T = TRUE, digits = 4) {

    # Load required packages (compatible with R < 4.5)
    if (!requireNamespace("lmtest", quietly = TRUE)) {
        stop("Package 'lmtest' required. Install with: install.packages('lmtest')")
    }
    if (!requireNamespace("sandwich", quietly = TRUE)) {
        stop("Package 'sandwich' required. Install with: install.packages('sandwich')")
    }

    stopifnot(is.vector(newey.west) && length(newey.west) == 1 && is.numeric(newey.west))
    stopifnot(is.vector(stdcoefs) && length(stdcoefs) == 1 && is.logical(stdcoefs))

    lmo <- stats::lm(..., x = TRUE, na.action = na.exclude)

    if (newey.west >= 0) {
        nw.se <- lmtest::coeftest(
            lmo,
            vcov = sandwich::NeweyWest(lmo, lag = newey.west, prewhite = FALSE)
        )[, 2]
    }

    if (stdcoefs) {
        xsd <- apply(lmo$x, 2, sd)
        ysd <- sd(lmo$model[, 1])
        stdcoefs.v <- lmo$coefficients * xsd / ysd
    }

    basesigma <- sigma(lmo)
    full.x.matrix <- lmo$x

    lmo <- stats::summary.lm(lmo)
    lmo$x <- full.x.matrix

    # Round t-statistics
    lmo$coefficients[, 3] <- round(lmo$coefficients[, 3], 2)
    if (truncate.T) {
        lmo$coefficients[, 3] <- iaw$winsorize.level(lmo$coefficients[, 3], -99.99, 99.99)
    }

    if (!keep.pval) {
        lmo$coefficients <- lmo$coefficients[, 1:3, drop = FALSE]
    }

    if (newey.west >= 0) {
        lmo$coefficients <- cbind(lmo$coefficients, nw.se)
        colnames(lmo$coefficients)[ncol(lmo$coefficients)] <- paste0(" se.nw(", newey.west, ")")
        lmo$coefficients <- cbind(lmo$coefficients, round(lmo$coefficients[, 1] / nw.se, 2))
        K <- ncol(lmo$coefficients)
        if (truncate.T) {
            lmo$coefficients[, K] <- iaw$winsorize.level(lmo$coefficients[, K], -99.99, 99.99)
        }
        colnames(lmo$coefficients)[K] <- paste0("T.nw(", newey.west, ")")
    }

    if (stdcoefs) {
        lmo$coefficients <- cbind(lmo$coefficients, stdcoefs.v)
        colnames(lmo$coefficients)[ncol(lmo$coefficients)] <- " stdcoefs"
    }

    if (include.anova) {
        aa <- data.frame(summary(aov(...))[[1]])[, 2, drop = FALSE][, 1]
        sumaa <- sum(aa)
        aa <- aa / sumaa
        lmo$residssq <- round(aa[length(aa)], digits)
        aa <- c(0.0, aa[1:(length(aa) - 1)])
        lmo$coefficients <- cbind(lmo$coefficients, aa)
        colnames(lmo$coefficients)[ncol(lmo$coefficients)] <- "pctsumsq"
    } else {
        lmo$residssq <- NA
    }

    lmo$coefficients <- round(lmo$coefficients, digits)
    colnames(lmo$coefficients)[1:3] <- c("coefest", "  se.ols", "T.ols")
    lmo$sigma <- round(basesigma, 8)

    lmo
}
