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
