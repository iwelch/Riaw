
#' ASSERT
#'
#' @name assert
#'
#' die if the condition fails.  we do abort here primarily to have the browser output
#'
#' @usage assert(condition, ...)
#'
#' @param condition boolean scalar
#' @param ... (e)strings
#'
#' @return never
#'

iaw$assert <- function (condition, ...) {
  if (!condition) {
      iaw$abort(...)
      stop()
  }
}
