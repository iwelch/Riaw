#' Extract Residuals
#'
#' @name residuals
#'
#' Extracts residuals from model.
#'
#' @param model Regression model (from \code{lm} or similar).
#' @param ... Additional arguments passed to \code{stats::residuals}.
#'
#' @return Residuals vector (with NAs preserved if \code{na.exclude} was used).
#'
#' @examples
#' # With na.exclude: residuals keep NA positions for alignment
#' df <- data.frame(y = c(1, NA, 3, 4, 5), x = c(1, 2, 3, 4, 5))
#' fit <- lm(y ~ x, data = df, na.action = na.exclude)
#' r <- iaw$residuals(fit)
#' length(r)  # 5: NA slot preserved at position 2
#'
#' # Without na.exclude: a warning is issued
#' fit2 <- lm(y ~ x, data = df)
#' r2 <- iaw$residuals(fit2)
#' length(r2)  # 4: NA row silently dropped
#'
#' @family regression
#' @export

iaw$residuals <- function(model, ...) {
    if (!any(grepl("na\\.exclude", model$call)))
        message("** please add na.action=na.exclude to your lm() call **")
    residuals(model, ...)
}
