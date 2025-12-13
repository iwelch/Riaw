
#' ENABLE.ERROR.LINE.NUM
#'
#' @name enable.error.line.num
#'
#' a reminder for debugging
#'
#' @usage enable.error.line.num ()
#'
#'  @seealso debug.on
#'


iaw$enable.error.line.num <- function () {
  options(error = quote({
    print(attr(dump.frames(), "error.message"))
    traceback()
  }))
}
