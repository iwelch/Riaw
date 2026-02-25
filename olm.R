
#' OLM
#'
#' @name olm
#'
#' Run an ordinary-least-squares model with enhanced output.
#'
#' @param ... Arguments passed to lm().
#' @param newey.west Lag for Newey-West standard errors. Default 0.
#' @param stdcoefs Include standardized coefficients. Default TRUE.
#' @param include.anova Include ANOVA sum of squares decomposition. Default TRUE.
#' @param keep.pval Keep p-values in output. Default FALSE.
#' @param truncate.T Truncate t-statistics to +/-99.99. Default TRUE.
#' @param digits Rounding digits. Default 4.
#'
#' @return An object of class "olm" inheriting from "summary.lm".
#'
#' @examples
#' # Basic regression
#' df <- data.frame(y = c(1, 2, 3, 4, 5), x = c(1.1, 1.9, 3.2, 3.8, 5.1))
#' fit <- iaw$olm(y ~ x, data = df)
#' print(fit)
#'
#' # With NA values (na.exclude keeps original indices in residuals)
#' df2 <- data.frame(y = c(1, NA, 3, 4, 5), x = c(1, 2, 3, 4, 5))
#' fit2 <- iaw$olm(y ~ x, data = df2)
#' print(fit2)
#'
#' # Multiple predictors; disable standardised coefficients
#' df3 <- data.frame(y = rnorm(20), x1 = rnorm(20), x2 = rnorm(20))
#' fit3 <- iaw$olm(y ~ x1 + x2, data = df3, stdcoefs = FALSE)
#' print(fit3)
#'
#' # Fama-MacBeth style cross-sectional regression
#' set.seed(99)
#' cs <- data.frame(ret = rnorm(30, 0.01, 0.03), beta = runif(30, 0.5, 1.5))
#' fit4 <- iaw$olm(ret ~ beta, data = cs)
#' fit4$coefficients[, "coefest"]  # intercept and risk premium
#'
#' # Newey-West standard errors for autocorrelated time-series residuals
#' set.seed(7)
#' ts_df <- data.frame(y = cumsum(rnorm(100)), x = 1:100)
#' fit5 <- iaw$olm(y ~ x, data = ts_df, newey.west = 3)
#' print(fit5)  # shows both OLS and NW t-statistics
#'
#' # Access R-squared and residual standard error programmatically
#' fit6 <- iaw$olm(y ~ x, data = data.frame(y = c(2, 4, 6), x = c(1, 2, 3)))
#' fit6$r.squared   # 1 (perfect fit)
#' fit6$sigma        # residual std error
#'
#' @family regression
#' @export
#'
#' @seealso print.olm

iaw$olm <- function (..., newey.west = 0, stdcoefs = TRUE, include.anova = TRUE,
                     keep.pval = FALSE, truncate.T = TRUE, digits = 4) {

    if (!requireNamespace("lmtest", quietly = TRUE)) stop("Package 'lmtest' required")
    if (!requireNamespace("sandwich", quietly = TRUE)) stop("Package 'sandwich' required")

    ## R is painfully error-tolerant. I prefer reasonable and immediate error warnings.
    stopifnot( (is.vector(newey.west))&(length(newey.west)==1)&(is.numeric(newey.west)) )
    stopifnot( (is.vector(stdcoefs))&(length(stdcoefs)==1)&(is.logical(stdcoefs)) )
    ## stopifnot( (is.vector(keepx))&(length(keepx)==1)|(is.logical(keepx)) )
    ## I wish I could check lm()'s argument, but I cannot.

    lmo <- stats::lm(..., x=TRUE, na.action=na.exclude)  ## keep NA values
    ## note that both the lmo$x matrix and residuals(lmo) omit all NA observations

    ## following two statements need to come early to apply to lmo (before we add stuff to it)

    if (newey.west >= 0) {
        nw.se <- lmtest::coeftest(lmo, vcov = sandwich::NeweyWest(lmo, lag = newey.west, prewhite = FALSE))[, 2]
    }
    if (stdcoefs) {
        xsd <- apply( lmo$x, 2, sd)
        ysd <- sd( lmo$model[,1] )
        stdcoefs.v <- lmo$coefficients*xsd/ysd
    }

    basesigma <- sigma(lmo)

    ## Compute ANOVA from lm object before converting to summary.lm
    if (include.anova) {
        anova_ss <- anova(lmo)[, "Sum Sq"]
    }

    full.x.matrix <- lmo$x
    lmo <- stats::summary.lm( lmo )  ## add the summary.lm object; changes contents of lmo
    lmo$x <- full.x.matrix

    ## 3 is the T stat
    lmo$coefficients[,3] <-round(lmo$coefficients[,3],2)
    if (truncate.T) lmo$coefficients[,3] <- iaw$winsorize.level( lmo$coefficients[,3], c(-99.99, 99.99) )

    if (!keep.pval) lmo$coefficients <- lmo$coefficients[,1:3]

    if (newey.west>=0) {
        lmo$coefficients <- cbind(lmo$coefficients, nw.se)
        colnames(lmo$coefficients)[ncol(lmo$coefficients)] <- paste0(" se.nw(",newey.west,")")
        lmo$coefficients <- cbind(lmo$coefficients, round(lmo$coefficients[,1]/nw.se,2))
        K <- ncol(lmo$coefficients)
        if (truncate.T) lmo$coefficients[,K] <- iaw$winsorize.level( lmo$coefficients[,K], c(-99.99, 99.99) )
        colnames(lmo$coefficients)[K] <- paste0("T.nw(",newey.west,")")
    }

    if (stdcoefs) {
        lmo$coefficients <- cbind(lmo$coefficients, stdcoefs.v )
        colnames(lmo$coefficients)[ncol(lmo$coefficients)] <- " stdcoefs"
    }

    if (include.anova) {
        aa <- anova_ss
        sumaa <- sum(aa)  ## includes residuals!!
        aa <- aa/sumaa
        lmo$residssq <- round(aa[length(aa)], digits)
        aa <- c(0.0, aa[1:(length(aa)-1)])  ## chops them off
        lmo$coefficients <- cbind(lmo$coefficients, aa )
        colnames(lmo$coefficients)[ncol(lmo$coefficients)] <- "pctsumsq"
    } else {
        lmo$residssq <- NA
    }

    lmo$coefficients <- round(lmo$coefficients,digits)
    colnames(lmo$coefficients)[1:3] <- c("coefest", "  se.ols", "T.ols")
    lmo$sigma <- round(basesigma,8)

    ## Assign class: "olm" first, then inherit from "summary.lm"
    class(lmo) <- c("olm", "summary.lm")

    ## Ensure print.olm is registered for S3 dispatch (survives cache load)
    if (!exists("print.olm", envir = globalenv(), inherits = FALSE))
        assign("print.olm", iaw$print.olm, envir = globalenv())

    lmo
}
