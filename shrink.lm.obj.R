
#' SHRINK.LM.OBJ
#'
#' @name shrink.lm.obj
#'
#'   save a smaller version of a lm that is still suitable for prediction
#'
#'  @details the point of save.lm is to delete all the data that went into the regression.  predict will then work only with newdata.  the main advantage is that it saves a lot of space on disk, because the qr object is HUGE.
#'
#'
#'  @usage save.lm (lmobject, file)
#'
#'  @param an lm object
#'
#'  @return an lm object without some of the very large components.
#'
#'  @seealso save
#'


iaw$shrink.lm.obj <- function( lmobject, file ) {
  (class(lmobject)== "lm") %or% "in save.lm, lmobject must be an lmobject, not a {{object(lmobject)}}.";
  for (el in c("residuals", "effects", "fitted.values","model")) lmobject[[el]] <- NULL
  lmobject$qr$qr <- NULL
}
