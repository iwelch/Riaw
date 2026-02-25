#' Newey-West Standard Errors
#'
#' @name neweywest
#'
#' Wrapper for sandwich::NeweyWest.
#'
#' @param model Linear model object.
#' @param lag Number of lags.
#'
#' @return Covariance matrix.
#'
#' @examples
#' # Compute Newey-West covariance matrix for a simple regression
#' df <- data.frame(y = cumsum(rnorm(30)), x = cumsum(rnorm(30)))
#' fit <- lm(y ~ x, data = df)
#' vcov_nw <- iaw$neweywest(fit, lag = 2)
#' sqrt(diag(vcov_nw))  # Newey-West standard errors
#'
#' # lag = 0 gives heteroskedasticity-consistent (HC) SEs
#' vcov_hc <- iaw$neweywest(fit, lag = 0)
#' sqrt(diag(vcov_hc))
#'
#' # Compare OLS vs Newey-West standard errors for autocorrelated residuals
#' set.seed(1)
#' n <- 100
#' x <- rnorm(n)
#' e <- filter(rnorm(n), 0.8, method = "recursive")  # AR(1) errors
#' y <- 1 + 2 * x + e
#' fit2 <- lm(y ~ x)
#' se_ols <- sqrt(diag(vcov(fit2)))        # naive SEs
#' se_nw  <- sqrt(diag(iaw$neweywest(fit2, lag = 4)))  # HAC SEs
#' se_nw / se_ols  # ratio > 1 indicates underestimated OLS SEs
#'
#' # Use floor(n^(1/3)) as a rule-of-thumb lag selection
#' nobs <- nobs(fit2)
#' auto_lag <- floor(nobs^(1/3))
#' iaw$neweywest(fit2, lag = auto_lag)
#'
#' @family regression
#' @export

iaw$neweywest <- function(model, lag = 0) {
    stopifnot(inherits(model, "lm"))
    stopifnot(is.numeric(lag), length(lag) == 1L)
    
    if (!requireNamespace("sandwich", quietly = TRUE)) {
        stop("Package 'sandwich' required")
    }
    sandwich::NeweyWest(model, lag = lag, prewhite = FALSE)
}
