#' Out-of-Sample Linear Model
#'
#' @name ooslm
#'
#' Computes recursive residuals and compares conditional vs unconditional
#' forecasting errors using strucchange::recresid.
#'
#' @param formula Regression formula.
#' @param data Data frame.
#' @param ... Additional arguments passed to recresid.
#'
#' @return Data frame with delta.abserr, uncondesterr, condesterr, to.predict.
#'
#' @family regression
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic usage: y is a linear trend plus noise, x is the trend variable
#' set.seed(1)
#' df <- data.frame(y = cumsum(rnorm(50)), x = 1:50)
#' res <- iaw$ooslm(y ~ x, df)
#' head(res)
#'
#' # Positive delta.abserr means the conditional model beats the unconditional mean
#' # Check what fraction of periods the model wins out-of-sample
#' mean(res$delta.abserr > 0, na.rm = TRUE)
#'
#' # Multiple predictors
#' set.seed(2)
#' n <- 60
#' df2 <- data.frame(y = rnorm(n), x1 = rnorm(n), x2 = rnorm(n))
#' res2 <- iaw$ooslm(y ~ x1 + x2, df2)
#' summary(res2$delta.abserr)
#'
#' # Evaluate an equity risk-premium model out-of-sample
#' set.seed(42)
#' n <- 120  # 10 years of monthly data
#' mkt <- data.frame(
#'   excess_ret = rnorm(n, 0.005, 0.04),
#'   dp_ratio   = cumsum(rnorm(n, 0, 0.01))
#' )
#' oos <- iaw$ooslm(excess_ret ~ dp_ratio, mkt)
#' mean(oos$delta.abserr > 0, na.rm = TRUE)  # fraction model wins OOS
#'
#' # Compare output columns
#' names(oos)  # "delta.abserr" "uncondesterr" "condesterr" "to.predict"
#'
#' # Visualise cumulative OOS performance advantage
#' plot(cumsum(ifelse(is.na(oos$delta.abserr), 0, oos$delta.abserr)),
#'      type = "l", ylab = "Cumulative advantage", xlab = "Period")
#' abline(h = 0, lty = 2)
#' }

iaw$ooslm <- function(formula, data, ...) {
    if (!requireNamespace("strucchange", quietly = TRUE)) {
        stop("Package 'strucchange' required")
    }

    condesterr.standardized <- strucchange::recresid(formula = formula, data = data, ...)
    K <- nrow(data) - length(condesterr.standardized)
    condesterr <- condesterr.standardized * sqrt(1.0 + 1.0 / (1:length(condesterr.standardized)))
    condesterr <- c(rep(NA, K), condesterr)

    to.predict <- data[[all.vars(formula)[1]]]

    uncondesterr <- sapply(1:nrow(data), function(i) to.predict[i + 1] - mean(to.predict[1:i]))
    uncondesterr <- head(c(NA, uncondesterr), -1)

    dr <- data.frame(
        delta.abserr = (abs(uncondesterr) - abs(condesterr)),
        uncondesterr = uncondesterr,
        condesterr = condesterr,
        to.predict = to.predict
    )
    rownames(dr) <- rownames(data)
    dr
}

#' @rdname ooslm
#' @keywords internal
iaw$ooslm.test <- function() {
    N <- 10
    set.seed(0)
    df <- data.frame(x = rnorm(N), y = (1:N)^2, z1 = rnorm(N) * 100, z2 = rnorm(N))
    formula <- y ~ z1 + z2
    iaw$ooslm(formula, df)
}
