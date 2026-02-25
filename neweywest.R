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
