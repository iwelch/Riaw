
#' DEBUG.ON
#'
#' @name debug.on
#'
#' start debugging (better error messages on error now)
#'
#' @usage debug.on ()
#'


iaw$debug.on <- function () {
  options(error = function(e) print(traceback(sapply(sys.calls(), deparse))))
}
