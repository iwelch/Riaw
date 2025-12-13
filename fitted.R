
#' FITTED
#'
#' @name fitted
#'
#' residuals, but NA values are not eliminated
#'
#' @usage iaw.fitted (lmobject)
#'
#' @param an lm object
#'
#' @return the fitted residuals.
#'
#' @seealso residuals
#'
## note: a more general way is `predict(lmobject, data= d)`

iaw$fitted <- function (lmobject, ...) {
    if (!any(grepl("na\\.exclude", lmobject$call))) message("
****************
**
** please add na.action=na.exclude to your lm call first!
**
****************
")

    stats::fitted(lmobject, ...)
}
