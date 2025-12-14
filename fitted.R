#' Extract Fitted Values Preserving NA Positions
#'
#' Extracts fitted values from an lm object, preserving NA positions if
#' \code{na.action=na.exclude} was used in the model.
#'
#' @param lmobject An object returned by \code{lm()}.
#' @param ... Additional arguments passed to \code{stats::fitted()}.
#'
#' @return A numeric vector of fitted values, with NA values in the positions
#'   where the original data had missing values (if na.exclude was used).
#'
#' @details
#' For NA positions to be preserved correctly, you must use
#' \code{na.action=na.exclude} in your \code{lm()} call, not \code{na.omit}.
#'
#' @export
#'
#' @seealso \code{\link{iaw$residuals}}, \code{\link{fitted}}, \code{\link{predict}}
#'
#' @examples
#' df <- data.frame(y = c(1, 2, NA, 4, 5), x = c(1, 2, 3, 4, 5))
#'
#' # Use na.exclude to preserve NA positions
#' model <- lm(y ~ x, data = df, na.action = na.exclude)
#' iaw$fitted(model)
#' # Returns vector of length 5 with NA in position 3

iaw$fitted <- function(lmobject, ...) {
    if (!any(grepl("na\\.exclude", lmobject$call))) {
        message("\n*** Please add na.action=na.exclude to your lm() call ***\n")
    }
    stats::fitted(lmobject, ...)
}
