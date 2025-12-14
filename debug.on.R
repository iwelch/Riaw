#' Enable Enhanced Error Debugging
#'
#' Sets R options to print detailed traceback information when errors occur.
#' Useful for debugging scripts and functions.
#'
#' @return Invisible NULL. Sets \code{options(error = ...)}.
#'
#' @export
#'
#' @seealso \code{\link{iaw$debug.advice}}, \code{\link{iaw$enable.error.line.num}},
#'   \code{\link{traceback}}
#'
#' @examples
#' \dontrun{
#' iaw$debug.on()
#' # Now errors will print full traceback
#' }

iaw$debug.on <- function() {
    options(error = function(e) print(traceback(sapply(sys.calls(), deparse))))
    invisible(NULL)
}
