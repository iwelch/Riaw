
#' RESIDUALS
#'
#' @name residuals
#'
#'  add a keep.NA option to residuals
#'
#'  @usage residuals(lmobject, keep.NA=FALSE, ...)
#'
#'  @param lmobject a linear model, usually obtained from lm
#'
#'  @return a vector with residuals of equal nrows as the number of observations in the model
#'

iaw$residuals <- function(lmobject, ...) {
    if (!any(grepl("na\\.exclude", lmobject$call))) message("
****************
**
** please add na.action=na.exclude to your lm call first!
**
****************
")

    stats:::residuals(lmobject,...)
}
