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
#' df <- data.frame(y = cumsum(rnorm(50)), x = 1:50)
#' iaw$ooslm(y ~ x, df)
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

iaw$ooslm.test <- function() {
    N <- 10
    set.seed(0)
    df <- data.frame(x = rnorm(N), y = (1:N)^2, z1 = rnorm(N) * 100, z2 = rnorm(N))
    formula <- y ~ z1 + z2
    iaw$ooslm(formula, df)
}
